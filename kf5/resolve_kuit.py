#!/usr/bin/env python
# -*- coding: UTF-8 -*-

# Resolve KUIT markup in i18n strings into plain or rich text.
#
# Usage:
#   resolve_kuit.py [OPTIONS] FILE_OR_DIRECTORY...
#
# Files are modified in-place. Modified file paths are written to stdout.
# If an argument is a directory, files from it are recursivelly collected.
# Only files with known extensions are processed (even if file with unknown
# extension is given directly in the command line, it will be ignored).
# The list of known extensions by resolution type can be listed with
# -k option. Option -s RESTYPE:EXT1[,EXT2...] can be used to register
# additional extensions (without leading dot, case ignored) for given
# resolution type. One extension may have several resolution types.
# Files in version control bookkeeping directories are skipped.
#
# In C-like function call files (resolution type 'ccall'),
# i18n strings are detected as arguments in calls with
# *i18n, *i18nc, *i18np, and *i18ncp function names.
# By default detection considers string arguments to be either single or
# double quoted, call arguments can be split into several lines, and
# strings are concatenated when separated only by whitespace.
# Default set of quotes can be replaced by repeating the -q QUOTE option.
#
# In XML-like markup files (resolution type 'xml'),
# i18n strings are detected as element texts, for a certain set of tags.
# i18n contexts are detected as attributes to those elements, for a certain
# set of attributes. These sets can be expanded using -T TAG1[,TAG2...]
# and -A ATTR1[,ATTR2...] options. Case is ignored for both.
# Markup inside the element text is expected to be XML-escaped (&lt;, etc.),
# i.e. the element text is first unescaped before resolution.
#
# In PO files (resolution type 'po'), i18n strings are detected 
# according to PO format.
# To process PO files, the Pology library must be ready for use.
# In msgstr fields, KUIT markup transformations for given language
# are looked up in its kdelibs4.po. The pattern path to kdelibs4.po files,
# which contains @lang@ placeholder, is given with -t PATTERN option.
# This can be a local path or a HTTP URL (e.g.
# http://websvn.kde.org/*checkout*/trunk/l10n-kde4/@lang@/messages/kdelibs/kdelibs4.po ).
# Language of processed PO file is determined from its Language: header field.
# If only PO files of one language are processed and they do not reliably
# contain this field, the language can be forced with -l LANG option.
# By default both the original and the translation fields are resolved,
# which is appropriate when the PO file is being resolved before
# it has been merged with new template resulting from the resolved code.
# If an unresolved PO file has been merged with new template first,
# then option -m should be issued to resolve only the translation fields.
# In this case, on fuzzy messages, if previous original fields (which are
# also resolved) and current original fields match after resolution,
# the message is unfuzzied.
#
# For a given i18n string, the decision of whether to convert KUIT markup
# into plain or Qt rich text is made based on the context marker,
# as described in KUIT documentation at
# http://techbase.kde.org/Development/Tutorials/Localization/i18n_Semantics .
# Target formats can also be manually specified for certain context markers
# by repeating the -f option. E.g. -f @info:progress=rich would override
# the default resolution into plain text for @info:progress i18n strings.
#
# NOTE: If <html> tags are added on rich text (see top_tag_res variable),
# then resolution must not be run over already resolved files.
# Context markers will remain but format modifiers will be removed from them,
# which may cause further modification in the second run.
#
# NOTE: If <numid> tags are simply removed (see numid_tag_res variable),
# a warning is issued on each removal to do something manually with
# its associated argument, e.g. wrap it in QString::number().
# It is probably best to look for <numid> tags and handle their arguments
# before running the resolution.

import locale
import optparse
import os
import re
import sys


def main ():

    opars = optparse.OptionParser(
        usage="%prog FILE_OR_DIRECTORY...",
        description="Resolve KUIT markup in i18n strings. "
                    "Files are recursively searched for, "
                    "and modified in place. "
                    "C-like i18n calls are looked for in all files, "
                    "except in PO files which are specially treated. "
                    "WARNING: Do not run twice over same files.")
    opars.add_option(
        "-f",
        dest="formats", action="append", default=[],
        metavar="MARKER=FORMAT",
        help="Set resolution into given target format for "
             "strings with this context marker. "
             "Target format can be one of: plain, rich. "
             "Option can be repeated.")
    opars.add_option(
        "-q",
        dest="quotes", action="append", default=[],
        metavar="QUOTE",
        help="Set opening and closing quote for string arguments "
             "in '%s' resolution type. "
             "Default is single and double quote. "
             "Option can be repeated." % "ccall")
    opars.add_option(
        "-s",
        dest="add_restype_exts", action="append", default=[],
        metavar="RESTYPE:EXT1[,EXT2...]",
        help="Set additional file name extension for given resolution type. "
             "Option can be repeated.")
    opars.add_option(
        "-T",
        dest="add_xml_texttags", action="store", default=None,
        metavar="TAG1[,TAG2...]",
        help="Set additional tags from which to collect text "
             "in '%s' resolution type." % "xml")
    opars.add_option(
        "-A",
        dest="add_xml_ctxtattrs", action="store", default=None,
        metavar="ATTR1[,ATTR2...]",
        help="Set additional attributes to consider as containing "
             "context in '%s' resolution type." % "xml")
    opars.add_option(
        "-t",
        dest="kdelibs4_path_pattern", action="store", default=None,
        metavar="PATH_PATTERN",
        help="The path pattern to kdelibs4.po files, "
             "which contains @lang@ placeholder. "
             "It can be a local path or HTTP URL. "
             "Needed only when processing PO files.")
    opars.add_option(
        "-l",
        dest="kdelibs4_lang", action="store", default=None,
        metavar="LANG",
        help="The language code of translated text in processed PO files, "
             "if it cannot be determined reliably from PO headers. "
             "When this option is in effect, PO files of exactly "
             "one language of translation must be proceesed.")
    opars.add_option(
        "-w",
        dest="msgfmt_wrap", action="store_true", default=False,
        help="Apply Gettext tools wrapping to PO files after resolving them.")
    opars.add_option(
        "-m",
        dest="post_merge", action="store_true", default=False,
        help="Resolve only translation fields in PO files. "
             "This is to be used when PO file is being resolved "
             "after it has been merged with template resulting "
             "from resolved code.")
    opars.add_option(
        "-I",
        dest="interface_wrap", action="store", default=None,
        metavar="HEAD_SEP",
        help="[undocumented]",
    )
    opars.add_option(
        "-k",
        dest="list_restypes", action="store_true", default=False,
        help="List known resolution types and associated file extensions. "
             "It will include additions by '%s' option." % "-s",
    )

    options, args = opars.parse_args()

    # Set additional resolution types.
    for rtextspec in options.add_restype_exts:
        lst = rtextspec.split(":", 1)
        if len(lst) != 2:
            raise StandardError(
                "Resolution specification '%s' given in command line "
                "is not valid." % rtextspec)
        rt, extspec = lst
        if rt not in _map_restype_ext:
            raise StandardError(
                "Unknown resolution type '%s' in resolution specification '%s' "
                "given in command line." % (rt, rtextspec))
        exts = [e.lower() for e in extspec.split(",")]
        _map_restype_ext[rt][0].update(exts)
    if options.list_restypes:
        for rt, (exts, rf, ons) in _map_restype_ext.items():
            report("%s: %s" % (rt, " ".join(sorted(exts))))
        exit(1)

    # Update target format by context marker specification.
    for fmtspec in options.formats:
        try:
            cmk, fmt = fmtspec.split("=", 1)
        except:
            raise StandardError(
                "Invalid target format specification '%s' "
                "given in command line." % fmtspec)
        if fmt not in _known_formats.values():
            raise StandardError(
                "Unknown target format '%s' given in command line." % fmt)
        _cmarker_to_format[cmk] = fmt

    # Set KUIT resolving options.
    if options.kdelibs4_path_pattern:
        if "@lang@" not in options.kdelibs4_path_pattern:
            raise StandardError(
                "Path pattern for kdelibs4.po files given in command line "
                "does not contain %s placeholder." % "@lang@")
    _kuit_spec.kdelibs4_path_pattern = options.kdelibs4_path_pattern
    _kuit_spec.force_lang = options.kdelibs4_lang
    _kuit_spec.interface_wrap = options.interface_wrap

    # Set C-call resolving options.
    if options.quotes:
        squotes = list(reversed(sorted(options.quotes))) # longest first
        _ccall_options.quotes[:] = squotes

    # Set XML resolving options.
    if options.add_xml_texttags:
        tags = options.add_xml_texttags.split(",")
        _xml_options.text_tags.update(tags)
    if options.add_xml_ctxtattrs:
        attrs = options.add_xml_ctxtattrs.split(",")
        _xml_options.ctxt_attrs[:0] = attrs # higher priority

    # Set PO resolving options.
    _po_options.msgfmt_wrap = options.msgfmt_wrap
    _po_options.post_merge = options.post_merge

    # Collect all files.
    file_paths = []
    for path in args:
        if os.path.isdir(path):
            for root, dirns, filens in os.walk(path):
                for filen in filens:
                    file_paths.append(os.path.join(root, filen))
        elif os.path.isfile(path):
            file_paths.append(path)
        else:
            raise StandardError(
                "Command line argument '%s' is neither a file "
                "nor a directory." % path)

    # Filter out VCS bookkeeping.
    mod_file_paths = []
    vcs_dirns = set(["CVS", ".svn", ".git"])
    for fp in file_paths:
        els = set(fp.split(os.path.sep))
        if not els.intersection(vcs_dirns):
            mod_file_paths.append(fp)
    file_paths = mod_file_paths

    # Resolve files.
    file_paths.sort()
    test_encs = ["utf8", "iso8859-1", "iso8859-15", "cp1252"]
    for fp in file_paths:
        rspecs = get_resolvers_for_file(fp)
        modified = False
        for restype, resolvef, onstring in rspecs:
            if onstring:
                fstr = open(fp, "rb").read()
                badpos = -1
                for fenc in test_encs:
                    try:
                        fstr = fstr.decode(fenc)
                    except UnicodeDecodeError, e:
                        if badpos < 0:
                            badpos = e.start
                    else:
                        badpos = -1
                        break
                if badpos < 0:
                    res_fstr = resolvef(fstr, fp)
                    if res_fstr != fstr:
                        tmpfp = fp + "~tmp"
                        fh = open(tmpfp, "wb")
                        fh.write(res_fstr.encode("utf8"))
                        fh.close()
                        os.rename(tmpfp, fp)
                        modified = True
                else:
                    warning("%s: Cannot decode file using any of "
                            "test encodings (UTF-8 try produces problem "
                            "in line %d, column %d), skipping it."
                            % (fp, lno_to(fstr, badpos), cno_to(fstr, badpos)))
            else:
                if resolvef(fp):
                    modified = True
        if modified:
            report(fp)


def report (msg):

    lenc = locale.getpreferredencoding()
    emsg = ("%s\n" % msg).encode(lenc)
    sys.stdout.write(emsg)


def warning (msg):

    lenc = locale.getpreferredencoding()
    emsg = ("[warning] %s\n" % msg).encode(lenc)
    sys.stderr.write(emsg)


class Data: pass
_kuit_spec = Data()
_kuit_spec.kdelibs4_path_pattern = None
_kuit_spec.force_lang = None
_kuit_spec.interface_wrap = None
_kuit_spec.langdata = {}


_space_in_place_tag_rx = re.compile(r"(<[^>]*\S)(/\s*>)", re.U | re.S)

def get_language_data (lang):

    langdata = _kuit_spec.langdata.get(lang)
    if langdata:
        return langdata

    kl4cat = None
    if lang != "en_US":
        # Fetch kdelibs4.po for this catalog's language.
        if not _kuit_spec.kdelibs4_path_pattern:
            raise StandardError(
                "Path pattern for kdelibs4.po not set (-t option).")
        kl4path = _kuit_spec.kdelibs4_path_pattern.replace("@lang@", lang)
        from urllib import urlopen
        kl4fh = urlopen(kl4path)
        from pology.catalog import Catalog
        kl4cat = Catalog("kdelibs4.po", readfh=kl4fh)

    langdata = Data()

    langdata.transform = {}
    for spec in _kuit_transforms.items():
        ktrkey, (msgctxt, msgid, subsmap, textmodf) = spec
        pattern = msgid
        if kl4cat is not None:
            msgs = kl4cat.select_by_key(msgctxt, msgid)
            if msgs and msgs[0].translated:
                pattern = msgs[0].msgstr[0]
        fmt = ktrkey[2]
        if fmt == "rich":
            # Add space before /> in in-place closed rich-text tags,
            # as Qt may fail to guess format as rich-text otherwise.
            pattern = _space_in_place_tag_rx.sub(r"\1 \2", pattern)
        tr = Data()
        tr.pattern = pattern
        tr.subsmap = subsmap
        tr.textmodf = textmodf
        langdata.transform[ktrkey] = tr

    langdata.shcdelim = {}
    for spec in _kuit_shortcut_delimiters.items():
        fmt, (msgctxt, msgid) = spec
        delim = msgid
        if kl4cat is not None:
            msgs = kl4cat.select_by_key(msgctxt, msgid)
            if msgs and msgs[0].translated:
                delim = msgs[0].msgstr[0]
        langdata.shcdelim[fmt] = delim

    langdata.keyname = {}
    for spec in _kuit_key_names:
        msgctxt, msgid = spec
        keyname = msgid
        if kl4cat is not None:
            msgs = kl4cat.select_by_key(msgctxt, msgid)
            if msgs and msgs[0].translated:
                keyname = msgs[0].msgstr[0]
        langdata.keyname[msgid] = keyname

    langdata.guidelim = {}
    for spec in _kuit_guipath_delimiters.items():
        fmt, (msgctxt, msgid) = spec
        delim = msgid
        if kl4cat is not None:
            msgs = kl4cat.select_by_key(msgctxt, msgid)
            if msgs and msgs[0].translated:
                delim = msgs[0].msgstr[0]
        langdata.guidelim[fmt] = delim

    langdata.ifacewrap = None
    if _kuit_spec.interface_wrap:
        langdata.ifacewrap = _kuit_spec.interface_wrap

    _kuit_spec.langdata[lang] = langdata
    return langdata


def lno_to (fstr, p):
    lno = fstr.count("\n", 0, p) + 1
    return lno


def cno_to (fstr, p):
    pb = fstr.rfind("\n", 0, p)
    # If no \n found, -1 is exactly what's needed below.
    cno = p - pb
    return cno


_ccall_options = Data()

# Call specification.
_ccall_options.calls = {
    # "callname": (ctxt_pos, text_pos, plural_pos)
    "i18n": (-1, 0, -1),
    "i18nc": (0, 1, -1),
    "i18np": (-1, 0, 1),
    "i18ncp": (0, 1, 2),
    "ki18n": (-1, 0, -1),
    "ki18nc": (0, 1, -1),
    "ki18np": (-1, 0, 1),
    "ki18ncp": (0, 1, 2),
    "I18N_NOOP": (-1, 0, -1),
    "I18N_NOOP2": (0, 1, -1),
    "I18N_NOOP2_NOSTRIP": (0, 1, -1),
}
# Equip with total number of strings.
_ccall_options.calls = dict([(cn, inds + (len([i for i in inds if i >= 0]),))
                             for cn, inds in _ccall_options.calls.items()])

# Default string quotes (must be sorted from longest to shortest).
_ccall_options.quotes = list(reversed(sorted([
    "\"",
    "'",
])))

# To-EOL and delimited comments which may mingle with
# concatenated string literals.
_ccall_options.midcstr_eolcmnts = set([
    "//", "#",
])
_ccall_options.midcstr_delimcmnts = set([
    ("/*", "*/"),
])

_ccall_head_rx = re.compile(r"([\w\d_]+)\s*\(", re.U | re.S)
_mask_chr = "\x04"
_print_mask_chr = u"¬"

def resolve_ccall (fstr, path):

    showparse = False
    if showparse:
        report("%s: >>>>> start >>>>>" % path)

    langdata = get_language_data("en_US")

    segs = []
    p1 = 0
    while True:
        m = _ccall_head_rx.search(fstr, p1)
        if not m:
            segs.append(fstr[p1:])
            break
        p2, p3 = m.span()
        segs.append(fstr[p1:p3])
        callname = m.group(1)
        callspec = _ccall_options.calls.get(callname)
        cseg = None
        if callspec:
            ictxt, itext, iplural, total = callspec
            p1a = p3
            argspecs = []
            all_strings = True
            end_call = False
            for k in range(total):
                if showparse:
                    report("%s:%d: iarg=%d  spos=%d"
                           % (path, lno_to(fstr, p1a), k, p1a))
                ret = _parse_cstr(fstr, p1a, (",", ")"), path,
                                  _ccall_options.midcstr_eolcmnts,
                                  _ccall_options.midcstr_delimcmnts)
                if not ret:
                    all_strings = False
                    break
                p2a, msarg, quote, outs = ret
                argspecs.append((msarg, quote, outs))
                p1a = p2a
                if outs[-1].endswith(")"):
                    end_call = True
                    break
            if len(argspecs) == total:
                if showparse:
                    report("%s:%d: call=[%s]%s"
                           % (path, lno_to(fstr, p3), callname,
                              "".join("{%s||%s}" % (_ppmasked(s[0]), s[1])
                                      for s in argspecs)))
                csegs = []
                lno = lno_to(fstr, p3)
                mctxt = argspecs[ictxt][0] if ictxt >= 0 else None
                for iarg, (msarg, quote, outs) in enumerate(argspecs):
                    if iarg != ictxt:
                        ret = resolve_kuit(mctxt, msarg, quote,
                                           langdata, path, lno)
                        res_mctxt, res_msarg = ret
                        res_sarg = _unmask(res_msarg, outs)
                        csegs.append(res_sarg)
                    else:
                        csegs.append("")
                if ictxt >= 0:
                    outs_ctxt = argspecs[ictxt][2]
                    res_ctxt = _unmask(res_mctxt, outs_ctxt)
                    csegs[ictxt] = res_ctxt
                if showparse:
                    report("%s:%d: res-segs=%s"
                           % (path, lno_to(fstr, p3),
                              "".join("{%s}" % s for s in csegs)))
                segs.append("".join(csegs))
                p3 = p1a
            elif all_strings and end_call:
                if showparse:
                    report("%s:%d: bad-call" % (path, lno_to(fstr, p3)))
                warning("%s:%d: Too little string arguments to call "
                        "(expected %d, got %d)."
                        % (path, lno_to(fstr, p3), total, len(argspecs)))
                p3 = p1a
            else:
                if showparse:
                    report("%s:%d: not-literal-call" % (path, lno_to(fstr, p3)))
        p1 = p3
    res_fstr = "".join(segs)
    if showparse:
        report("%s: <<<<< end <<<<<" % path)
    return res_fstr


def _ppmasked (s):

    return s.replace(_mask_chr, _print_mask_chr)


def _unmask (ms, outs):

    segs = []
    p1 = 0
    io = 0
    while True:
        p2 = ms.find(_mask_chr, p1)
        if p2 < 0:
            segs.append(ms[p1:])
            break
        segs.append(ms[p1:p2])
        segs.append(outs[io])
        io += 1
        p1 = p2 + len(_mask_chr)
    s = "".join(segs)
    return s


def _parse_cstr (fstr, spos, ends, path=None, eolcmnts=[], delimcmnts=[]):

    showparse = False

    l = len(fstr)
    p = spos
    if showparse:
        report("parse-cstr-start %d" % p)
    segs = []
    outs = []
    quote = None
    while True:
        pp = p
        while p < l and fstr[p].isspace():
            p += 1
        segs.append(_mask_chr)
        outs.append(fstr[pp:p])
        if p == l:
            break
        at_quote = False
        if quote is None:
            for q in _ccall_options.quotes:
                if fstr[p:p + len(q)] == q:
                    at_quote = True
                    quote = q
                    lq = len(quote)
                    break
        else:
            if fstr[p:p + lq] == quote:
                at_quote = True
        if at_quote:
            pp = p
            p += lq
            p = find_esc(fstr, quote, "\\", p)
            if p < 0:
                if path:
                    warning("%s:%d: Unterminated string literal."
                            % (path, lno_to(fstr, pp)))
                return None
            p += lq
            segs.append(fstr[pp:p])
            if showparse:
                report("parse-cstr-quote-end %d" % p)
            continue
        at_end = False
        for end in ends:
            if fstr[p:p + len(end)] == end:
                pp = p
                p += len(end)
                at_end = True
                segs.append(_mask_chr)
                outs.append(fstr[pp:p])
                if showparse:
                    report("parse-cstr-end-end %d" % p)
                break
        if at_end:
            break
        cmnt_end = False
        for ec in eolcmnts:
            if fstr[p:p + len(ec)] == ec:
                pp = p
                p += len(ec)
                while p < l and fstr[p] != "\n":
                    p += 1
                if p < l:
                    p += 1
                cmnt_end = True
                segs.append(_mask_chr)
                outs.append(fstr[pp:p])
                if showparse:
                    report("parse-cstr-eol-cmnt-end %d" % p)
                break
        if cmnt_end:
            continue
        for dc1, dc2 in delimcmnts:
            if fstr[p:p + len(dc1)] == dc1:
                pp = p
                p += len(dc1)
                while p < l and fstr[p:p + len(dc2)] != dc2:
                    p += 1
                if p == l:
                    warning("%s:%d: Unterminated comment."
                            % (path, lno_to(fstr, pp)))
                    return None
                p += len(dc2)
                cmnt_end = True
                segs.append(_mask_chr)
                outs.append(fstr[pp:p])
                if showparse:
                    report("parse-cstr-delim-cmnt-end %d" % p)
                break
        if cmnt_end:
            continue
        break
    if quote is None:
        return None

    mstr = "".join(segs)
    return p, mstr, quote, outs


_xml_options = Data()

# Default tags and attributes to extract from.
# Ordering of attributes is significant, first found is taken as context.
# According to extractrc from kdesdk/scripts/.
_xml_options.text_tags = set([
    "text", "title", "string", "whatsthis", "tooltip", "label",
])
_xml_options.ctxt_attrs = [
    "context", "comment",
]

_xml_rx = Data()
_xml_rx.inited = False
def _init_xml_regexes ():
    if _xml_rx.inited:
        return
    tagins = "|".join(sorted(_xml_options.text_tags))
    rx = re.compile(r"<\s*(%s)\b([^>]*)>([^<]*)<\s*/\s*\1\s*>" % tagins,
                    re.U | re.S | re.I)
    _xml_rx.i18n_el = rx
    attrins = "|".join(_xml_options.ctxt_attrs)
    rx = re.compile(r"""^(.*\b(?:%s)\s*=\s*['"])(.*?)(['"].*)$""" % attrins,
                    re.U | re.S | re.I)
    _xml_rx.ctxt_attr = rx
    _xml_rx.inited = True


def resolve_xml (fstr, path):

    showparse = False
    if showparse:
        report("%s: >>>>> start >>>>>" % path)

    _init_xml_regexes()
    langdata = get_language_data("en_US")

    segs = []
    p1 = 0
    while True:
        m = _xml_rx.i18n_el.search(fstr, p1)
        if not m:
            segs.append(fstr[p1:])
            break
        p2, p3 = m.span()
        lno = lno_to(fstr, p2)
        segs.append(fstr[p1:p2])
        tag, attr_str, etext = m.groups()
        ctxt = None
        m = _xml_rx.ctxt_attr.search(attr_str)
        if m:
            attr_head, ectxt, attr_tail = m.groups()
            ctxt, noesc_ctxt = unescape_xml(ectxt, testnoesc=True)
        text, noesc_text = unescape_xml(etext, testnoesc=True)
        if showparse:
            if ctxt is not None:
                report("%s:%d: ctxt-text={%s}{%s}" % (path, lno, ectxt, etext))
            else:
                report("%s:%d: text={%s}" % (path, lno, etext))
        ret = resolve_kuit(ctxt, text, None, langdata, path, lno)
        res_ctxt, res_text = ret
        res_etext = escape_xml(res_text, noesc=noesc_text)
        if ctxt is not None:
            res_ectxt = escape_xml(res_ctxt, noesc=noesc_ctxt)
            seg = ("<%s%s%s%s>%s</%s>"
                   % (tag, attr_head, res_ectxt, attr_tail, res_etext, tag))
        else:
            seg = "<%s%s>%s</%s>" % (tag, attr_str, res_etext, tag)
        if showparse:
            if ctxt is not None:
                report("%s:%d: res-ctxt-text={%s}{%s}"
                       % (path, lno, res_ectxt, res_etext))
            else:
                report("%s:%d: res-text={%s}" % (path, lno, res_etext))
        segs.append(seg)
        p1 = p3
    res_fstr = "".join(segs)

    if showparse:
        report("%s: <<<<< end <<<<<" % path)
    return res_fstr


_po_options = Data()
_po_options.msgfmt_wrap = False

def resolve_po (path):

    from pology.catalog import Catalog
    from pology.gtxtools import msgfilter

    cat = Catalog(path)

    langdata_src = get_language_data("en_US")
    lang = _kuit_spec.force_lang or cat.language()
    if not lang:
        raise StandardError(
            "%s: Cannot determine language of PO file." % path)
    langdata_trn = get_language_data(lang)

    seen_keys = set()
    for ind, msg in enumerate(cat):
        # Previous original fields.
        ctxt_prev = msg.msgctxt_previous
        has_previous = False
        if msg.msgid_previous is not None:
            has_previous = True
            ret = resolve_kuit(ctxt_prev, msg.msgid_previous, None,
                               langdata_src, path, msg.refline)
            msg.msgid_previous = ret[1]
            if ctxt_prev is not None:
                msg.msgctxt_previous = ret[0]
            if msg.msgid_plural_previous is not None:
                ret = resolve_kuit(ctxt_prev, msg.msgid_plural_previous, None,
                                   langdata_src, path, msg.refline)
                msg.msgid_plural_previous = ret[1]
        ctxt = msg.msgctxt
        if not _po_options.post_merge:
            # Original fields.
            ret = resolve_kuit(ctxt, msg.msgid, None,
                               langdata_src, path, msg.refline)
            msg.msgid = ret[1]
            if ctxt is not None:
                msg.msgctxt = ret[0]
            if msg.msgid_plural is not None:
                ret = resolve_kuit(ctxt, msg.msgid_plural, None,
                                   langdata_src, path, msg.refline)
                msg.msgid_plural = ret[1]
        # Translation fields.
        ctxt_trn = ctxt if (not msg.fuzzy or not has_previous) else ctxt_prev
        for i in range(len(msg.msgstr)):
            ret = resolve_kuit(ctxt_trn, msg.msgstr[i], None,
                               langdata_trn, path, msg.refline)
            msg.msgstr[i] = ret[1]
            if msg.msgid.endswith("\n") and not msg.msgstr[i].endswith("\n"):
                msg.msgstr[i] += "\n"
        # In post-merge mode, maybe it can be unfuzzied now.
        if _po_options.post_merge and msg.fuzzy:
            if (    msg.msgctxt == msg.msgctxt_previous
                and msg.msgid == msg.msgid_previous
                and msg.msgid_plural == msg.msgid_plural_previous
            ):
                msg.unfuzzy()
        # Conversion may make a message with same key as a previous one,
        # remove the current message in that case.
        if msg.key in seen_keys:
            cat.remove_on_sync(ind)
        else:
            seen_keys.add(msg.key)

    modified = cat.sync()
    if modified and _po_options.msgfmt_wrap:
        msgfilter(["cat"])(cat.filename)

    return modified


_map_restype_ext = {
    "ccall": (set([
        "cpp", "cxx", "cc", "c",
        "h", "hpp", "hxx", "hh",
        "py", "js", "rb", "qml",
        #"kcfg", won't work due to XML escaping; but there is
        # no existing case of embedded i18n() with KUIT in KDE repos.
    ]), resolve_ccall, True),

    "xml": (set([
        "ui", "rc", "kcfg",
    ]), resolve_xml, True),

    "po": (set([
        "po", "pot",
    ]), resolve_po, False),
}
# Inverted resolution types by extension.
_map_ext_restype = {}
def _init_map_ext_restype ():
    if _map_ext_restype:
        return
    for rt, (exts, rf, ons) in _map_restype_ext.items():
        for ext in exts:
            if ext not in _map_ext_restype:
                _map_ext_restype[ext] = []
            _map_ext_restype[ext].append((rt, rf, ons))


def get_resolvers_for_file (path):

    _init_map_ext_restype()
    p = path.rfind(".")
    if p >= 0:
        ext = path[p + 1:]
    else:
        ext = ""
    rspecs = _map_ext_restype.get(ext, [])
    return rspecs


# KUIT keyboard shortcut delimiters and lookup key in PO files, as
# format: (msgctxt, msgid).
# According to kuitsemantics.cpp from kdecore.
_kuit_raw_shortcut_delimiter_rx = re.compile(r"\+|-", re.U)
_kuit_shortcut_delimiters = {
    "plain": (u"shortcut-key-delimiter/plain", u"+"),
    "rich": (u"shortcut-key-delimiter/rich", u"+"),
}
# Add delimiters for term format, same as plain.
_kuit_shortcut_delimiters["term"] = _kuit_shortcut_delimiters["plain"]

# KUIT keyboard key names and lookup in PO files,
# as set((msgctxt, msgid)). F%1 is special.
_kuit_key_names_raw = set([
    u"Alt", u"AltGr", u"Backspace", u"CapsLock", u"Control", u"Ctrl",
    u"Del", u"Delete", u"Down", u"End", u"Enter", u"Esc", u"Escape",
    u"Home", u"Hyper", u"Ins", u"Insert", u"Left", u"Menu", u"Meta",
    u"NumLock", u"PageDown", u"PageUp", u"PgDown", u"PgUp", u"PauseBreak",
    u"PrintScreen", u"PrtScr", u"Return", u"Right", u"ScrollLock", u"Shift",
    u"Space", u"Super", u"SysReq", u"Tab", u"Up", u"Win", u"F%1",
])
_kuit_key_names = set((u"keyboard-key-name", kn) for kn in _kuit_key_names_raw)

def textmod_shortcut (text, quote, fmt, langdata):

    segs = []
    p1 = 0
    while True:
        m = _kuit_raw_shortcut_delimiter_rx.search(text, p1)
        if not m:
            keyname = text[p1:].strip()
        else:
            p2, p3 = m.span()
            keyname = text[p1:p2].strip()
        if keyname[:1] == "F" and keyname[1:].isdigit():
            lkeypattern = langdata.keyname.get(u"F%1", u"F%1")
            lkeyname = lkeypattern.replace("%1", keyname[1:])
        else:
            lkeyname = langdata.keyname.get(keyname, keyname)
        segs.append(lkeyname)
        if not m:
            break
        segs.append(langdata.shcdelim[fmt])
        p1 = p3
    res_text = "".join(segs)
    if quote:
        res_text = escape_c(res_text, quote)
    return res_text


# KUIT UI path delimiters and lookup key in PO files, as
# format: (msgctxt, msgid).
# According to kuitsemantics.cpp from kdecore.
_kuit_raw_guipath_delimiter_rx = re.compile(r"->|\|", re.U)
_kuit_guipath_delimiters = {
    "plain": (u"gui-path-delimiter/plain", u"→"),
    "rich": (u"gui-path-delimiter/rich", u"→"),
}
# Add delimiters for term format, same as plain.
_kuit_guipath_delimiters["term"] = _kuit_guipath_delimiters["plain"]

def textmod_interface (text, quote, fmt, langdata):

    segs = []
    p1 = 0
    while True:
        m = _kuit_raw_guipath_delimiter_rx.search(text, p1)
        if not m:
            pathel = text[p1:].strip()
        else:
            p2, p3 = m.span()
            pathel = text[p1:p2].strip()
        if langdata.ifacewrap:
            head, sep = langdata.ifacewrap[:-1], langdata.ifacewrap[-1:]
            pathel = "%s%s%s" % (head, pathel, sep)
        segs.append(pathel)
        if not m:
            break
        segs.append(langdata.guidelim[fmt])
        p1 = p3
    res_text = "".join(segs)
    if quote:
        res_text = escape_c(res_text, quote)
    return res_text


# KUIT transformation patterns and lookup key in PO files, as
# (tag, attributes, format): (msgctxt, msgid, subsmap, textmodf).
# According to kuitsemantics.cpp from kdecore.
_kuit_transforms = {
    (u"title", frozenset([]), "plain"):
        (u"@title/plain",
         u"== %1 ==",
         {"%1": "title"},
         None),
    (u"title", frozenset([]), "rich"):
        (u"@title/rich",
         u"<h2>%1</h2>",
         {"%1": "title"},
         None),
    (u"subtitle", frozenset([]), "plain"):
        (u"@subtitle/plain",
         u"~ %1 ~",
         {"%1": "subtitle"},
         None),
    (u"subtitle", frozenset([]), "rich"):
        (u"@subtitle/rich",
         u"<h3>%1</h3>",
         {"%1": "subtitle"},
         None),
    (u"para", frozenset([]), "plain"):
        (u"@para/plain",
         u"%1",
         {"%1": "para"},
         None),
    (u"para", frozenset([]), "rich"):
        (u"@para/rich",
         u"<p>%1</p>",
         {"%1": "para"},
         None),
    (u"list", frozenset([]), "plain"):
        (u"@list/plain",
         u"%1",
         {"%1": "list"},
         None),
    (u"list", frozenset([]), "rich"):
        (u"@list/rich",
         u"<ul>%1</ul>",
         {"%1": "list"},
         None),
    (u"item", frozenset([]), "plain"):
        (u"@item/plain",
         u"  * %1",
         {"%1": "item"},
         None),
    (u"item", frozenset([]), "rich"):
        (u"@item/rich",
         u"<li>%1</li>",
         {"%1": "item"},
         None),
    (u"note", frozenset([]), "plain"):
        (u"@note/plain",
         u"Note: %1",
         {"%1": "note"},
         None),
    (u"note", frozenset([]), "rich"):
        (u"@note/rich",
         u"<i>Note</i>: %1",
         {"%1": "note"},
         None),
    (u"note", frozenset([u"label"]), "plain"):
        (u"@note-with-label/plain\n"
         u"%1 is the note label, %2 is the text",
         u"%1: %2",
         {"%1": "label", "%2": "note"},
         None),
    (u"note", frozenset([u"label"]), "rich"):
        (u"@note-with-label/rich\n"
         u"%1 is the note label, %2 is the text",
         u"<i>%1</i>: %2",
         {"%1": "label", "%2": "note"},
         None),
    (u"warning", frozenset([]), "plain"):
        (u"@warning/plain",
         u"WARNING: %1",
         {"%1": "warning"},
         None),
    (u"warning", frozenset([]), "rich"):
        (u"@warning/rich",
         u"<b>Warning</b>: %1",
         {"%1": "warning"},
         None),
    (u"warning", frozenset([u"label"]), "plain"):
        (u"@warning-with-label/plain\n"
         u"%1 is the warning label, %2 is the text",
         u"%1: %2",
         {"%1": "label", "%2": "warning"},
         None),
    (u"warning", frozenset([u"label"]), "rich"):
        (u"@warning-with-label/rich\n"
         u"%1 is the warning label, %2 is the text",
         u"<b>%1</b>: %2",
         {"%1": "label", "%2": "warning"},
         None),
    (u"link", frozenset([]), "plain"):
        (u"@link/plain",
         u"%1",
         {"%1": "link"},
         None),
    (u"link", frozenset([]), "rich"):
        (u"@link/rich",
         u"<a href=\"%1\">%1</a>",
         {"%1": "link"},
         None),
    (u"link", frozenset([u"url"]), "plain"):
        (u"@link-with-description/plain\n"
         u"%1 is the URL, %2 is the descriptive text",
         u"%2 (%1)",
         {"%2": "link", "%1": "url"},
         None),
    (u"link", frozenset([u"url"]), "rich"):
        (u"@link-with-description/rich\n"
         u"%1 is the URL, %2 is the descriptive text",
         u"<a href=\"%1\">%2</a>",
         {"%2": "link", "%1": "url"},
         None),
    (u"filename", frozenset([]), "plain"):
        (u"@filename/plain",
         u"‘%1’",
         {"%1": "filename"},
         None),
    (u"filename", frozenset([]), "rich"):
        (u"@filename/rich",
         u"<tt>%1</tt>",
         {"%1": "filename"},
         None),
    (u"application", frozenset([]), "plain"):
        (u"@application/plain",
         u"%1",
         {"%1": "application"},
         None),
    (u"application", frozenset([]), "rich"):
        (u"@application/rich",
         u"%1",
         {"%1": "application"},
         None),
    (u"command", frozenset([]), "plain"):
        (u"@command/plain",
         u"%1",
         {"%1": "command"},
         None),
    (u"command", frozenset([]), "rich"):
        (u"@command/rich",
         u"<tt>%1</tt>",
         {"%1": "command"},
         None),
    (u"command", frozenset([u"section"]), "plain"):
        (u"@command-with-section/plain\n"
         u"%1 is the command name, %2 is its man section",
         u"%1(%2)",
         {"%1": "command", "%2": "section"},
         None),
    (u"command", frozenset([u"section"]), "rich"):
        (u"@command-with-section/rich\n"
         u"%1 is the command name, %2 is its man section",
         u"<tt>%1(%2)</tt>",
         {"%1": "command", "%2": "section"},
         None),
    (u"resource", frozenset([]), "plain"):
        (u"@resource/plain",
         u"“%1”",
         {"%1": "resource"},
         None),
    (u"resource", frozenset([]), "rich"):
        (u"@resource/rich",
         u"“%1”",
         {"%1": "resource"},
         None),
    (u"icode", frozenset([]), "plain"):
        (u"@icode/plain",
         u"“%1”",
         {"%1": "icode"},
         None),
    (u"icode", frozenset([]), "rich"):
        (u"@icode/rich",
         u"<tt>%1</tt>",
         {"%1": "icode"},
         None),
    (u"bcode", frozenset([]), "plain"):
        (u"@bcode/plain",
         u"\n%1\n",
         {"%1": "bcode"},
         None),
    (u"bcode", frozenset([]), "rich"):
        (u"@bcode/rich",
         u"<pre>%1</pre>",
         {"%1": "bcode"},
         None),
    (u"shortcut", frozenset([]), "plain"):
        (u"@shortcut/plain",
         u"%1",
         {"%1": "shortcut"},
         textmod_shortcut),
    (u"shortcut", frozenset([]), "rich"):
        (u"@shortcut/rich",
         u"<b>%1</b>",
         {"%1": "shortcut"},
         textmod_shortcut),
    (u"interface", frozenset([]), "plain"):
        (u"@interface/plain",
         u"|%1|",
         {"%1": "interface"},
         textmod_interface),
    (u"interface", frozenset([]), "rich"):
        (u"@interface/rich",
         u"<i>%1</i>",
         {"%1": "interface"},
         textmod_interface),
    (u"emphasis", frozenset([]), "plain"):
        (u"@emphasis/plain",
         u"*%1*",
         {"%1": "emphasis"},
         None),
    (u"emphasis", frozenset([]), "rich"):
        (u"@emphasis/rich",
         u"<i>%1</i>",
         {"%1": "emphasis"},
         None),
    (u"emphasis", frozenset([u"strong"]), "plain"):
        (u"@emphasis-strong/plain",
         u"**%1**",
         {"%1": "emphasis"},
         None),
    (u"emphasis", frozenset([u"strong"]), "rich"):
        (u"@emphasis-strong/rich",
         u"<b>%1</b>",
         {"%1": "emphasis"},
         None),
    (u"placeholder", frozenset([]), "plain"):
        (u"@placeholder/plain",
         u"&lt;%1&gt;",
         {"%1": "placeholder"},
         None),
    (u"placeholder", frozenset([]), "rich"):
        (u"@placeholder/rich",
         u"&lt;<i>%1</i>&gt;",
         {"%1": "placeholder"},
         None),
    (u"email", frozenset([]), "plain"):
        (u"@email/plain",
         u"&lt;%1&gt;",
         {"%1": "email"},
         None),
    (u"email", frozenset([]), "rich"):
        (u"@email/rich",
         u"&lt;<a href=\"mailto:%1\">%1</a>&gt;",
         {"%1": "email"},
         None),
    (u"email", frozenset([u"address"]), "plain"):
        (u"@email-with-name/plain\n"
         u"%1 is name, %2 is address",
         u"%1 &lt;%2&gt;",
         {"%1": "email", "%2": "address"},
         None),
    (u"email", frozenset([u"address"]), "rich"):
        (u"@email-with-name/rich\n"
         u"%1 is name, %2 is address",
         u"<a href=\"mailto:%2\">%1</a>",
         {"%1": "email", "%2": "address"},
         None),
    (u"envar", frozenset([]), "plain"):
        (u"@envar/plain",
         u"$%1",
         {"%1": "envar"},
         None),
    (u"envar", frozenset([]), "rich"):
        (u"@envar/rich",
         u"<tt>$%1</tt>",
         {"%1": "envar"},
         None),
    (u"message", frozenset([]), "plain"):
        (u"@message/plain",
         u"/%1/",
         {"%1": "message"},
         None),
    (u"message", frozenset([]), "rich"):
        (u"@message/rich",
         u"<i>%1</i>",
         {"%1": "message"},
         None),
    (u"nl", frozenset([]), "plain"):
        (u"@nl/plain",
         u"%1\n",
         {"%1": "nl"},
         None),
    (u"nl", frozenset([]), "rich"):
        (u"@nl/rich",
         u"%1<br/>",
         {"%1": "nl"},
         None),
}

# Add patterns for term format, same as plain.
for (tag, attrs, fmt), trspec in _kuit_transforms.items():
    if fmt == "plain":
        _kuit_transforms[(tag, attrs, "term")] = trspec

# Collect all known tags and formats.
_kuit_tags = set()
_known_formats = set()
for (tag, attrs, fmt), trspec in _kuit_transforms.items():
    _kuit_tags.add(tag)
    _known_formats.add(fmt)

# Qt rich text tags (used for implicit determination of rich format).
_html_tags = set([
    "a", "address", "b", "big", "blockquote", "body", "br",
    "center", "cita", "code", "dd", "dfn", "div", "dl", "dt", "em",
    "font", "h1", "h2", "h3", "h4", "h5", "h6", "head", "hr", "html",
    "i", "img", "kbd", "meta", "li", "nobr", "ol", "p", "pre",
    "qt", "s", "samp", "small", "span", "strong", "sup", "sub",
    "table", "tbody", "td", "tfoot", "th", "thead", "title", "tr", "tt",
    "u", "ul", "var",
])

# Default target formats by context marker.
# According to kuitsemantics.cpp from kdecore.
_cmarker_to_format = {
    "@action": "plain",
    "@title": "plain",
    "@label": "plain",
    "@option": "plain",
    "@item": "plain",
    "@info": "rich",
    "@info:progress": "plain",
    "@info:status": "plain",
    "@info:credit": "plain",
    "@info:shell": "plain",
}

_top_tag_rx = re.compile(r"<\s*(qt|html)\b[^>]*>(.*)<\s*/\s*qt\s*>",
                         re.U | re.S | re.I)

def resolve_kuit (ctxt, text, quote, langdata, path, lno):

    fmt_exp, res_ctxt, has_cmarker = format_from_cmarker(ctxt, quote)
    if fmt_exp and fmt_exp not in _known_formats:
        warning("%s:%d: Unknown explicit format modifier '%s'."
                % (path, lno, fmt_exp))
        return ctxt, text
    fmt = fmt_exp or format_from_tags(text, quote) or "plain"

    ret = _resolve_kuit_r(text, quote, fmt, langdata, path, lno)
    res_text, has_any_kuit_tag, has_any_html_tag, has_top_tag = ret

    if fmt_exp != "rich" and not has_any_html_tag:
        ret = resolve_entities(res_text, path, lno)
        res_text, any_entity_resolved = ret
    else:
        any_entity_resolved = False

    if not has_cmarker and not has_any_kuit_tag and not any_entity_resolved:
        # In this case the resolution should have been no-op,
        # so return the original input just in case.
        return ctxt, text

    if has_top_tag or fmt_exp == "rich":
        # What to do with top tag in rich text.
        # 0 - As in KUIT processing in kdecore. But this would cause
        #     <html> tags to appear in otherwise plain text which happens
        #     to be sent to rich-text capable output. People may not like it.
        #     (It would also cause that running resolution over already
        #     resolved files leads to spurious additon of <html> tags,
        #     e.g. 1st resolution @info/plain -> @info and no <html> tag,
        #     2nd resolution @info -> @info and <html> tag.)
        # 1 - Original top tag is removed and then <html> tag added only if
        #     there is another tag or entity in the text.
        # 2 - Top tag is neither added nor removed, but left as it is
        #     in the literal text.
        top_tag_res = 2
        if top_tag_res in (0, 1):
            if has_top_tag:
                res_text = _top_tag_rx.sub(r"\2", res_text)
            if top_tag_res == 0 or ("<" in res_text or "&" in res_text):
                p1 = 0
                p2 = len(res_text)
                if quote:
                    p1 = res_text.find(quote) + len(quote)
                    p2 = res_text.rfind(quote)
                res_text = ("%s<html>%s</html>%s"
                            % (res_text[:p1], res_text[p1:p2], res_text[p2:]))
        elif top_tag_res == 2:
            pass
        else:
            raise StandardError("Unknown top tag resolution choice '%d'."
                                % top_tag_res)

    return res_ctxt, res_text


_element_rx = re.compile(r"<\s*(\w+)(?:([^>]*)>(.*?)<\s*/\s*\1|\s*/)\s*>",
                         re.U | re.S)
_attribute_rx = re.compile(r"""\b(\w+)\s*=\s*["'](.*?)["']""")

def _resolve_kuit_r (text, quote, fmt, langdata, path, lno):

    segs = []
    p1 = 0
    has_any_kuit_tag = False
    has_any_html_tag = False
    has_top_tag = False
    while True:
        m = _element_rx.search(text, p1)
        if not m:
            segs.append(text[p1:])
            break
        p2, p3 = m.span()
        segs.append(text[p1:p2])
        tag, attrstr, etext = m.groups()
        if etext is None:
            in_place = True
            attrstr, etext = "", ""
        else:
            in_place = False
        ret = _resolve_kuit_r(etext, quote, fmt, langdata, path, lno)
        res_etext, has_any_kuit_tag_1, has_any_html_tag_1, has_top_tag_1 = ret
        has_any_html_tag = has_any_html_tag or has_any_html_tag_1
        has_any_kuit_tag = has_any_kuit_tag or has_any_kuit_tag_1
        res_span = text[p2:p3] # in case no other resolution
        if tag in _kuit_tags:
            has_any_kuit_tag = True
            attrmap = dict(_attribute_rx.findall(attrstr))
            has_top_tag = has_top_tag or has_top_tag_1
            trkey = (tag, frozenset(attrmap.keys()), fmt)
            tr = langdata.transform.get(trkey)
            if tr is not None:
                if tr.textmodf:
                    res_etext = tr.textmodf(res_etext, quote, fmt, langdata)
                res_span = tr.pattern
                if quote:
                    res_span = escape_c(res_span, quote)
                replmap = attrmap
                replmap[tag] = res_etext
                # Replace in one pass, because replacement might contain %N.
                p1a = 0
                csegs = []
                seen_pls = set()
                while True:
                    p2a = res_span.find("%", p1a)
                    if p2a < 0:
                        csegs.append(res_span[p1a:])
                        break
                    csegs.append(res_span[p1a:p2a])
                    if res_span[p2a + 1:p2a + 2].isdigit():
                        pl = res_span[p2a:p2a + 2]
                        nm = tr.subsmap[pl]
                        cseg = replmap[nm] # cannot fail
                        if quote and pl in seen_pls:
                            # If placeholder was already replaced once,
                            # further replacements have to eliminate
                            # masking chars and quotes, because
                            # total number of masking chars must not change.
                            cseg = join_quoted(cseg, quote,
                                               invert=True, strip=True)
                        seen_pls.add(pl)
                        csegs.append(cseg)
                        p1a = p2a + 2
                    else:
                        csegs.append("%")
                        p1a = p2a + 1
                res_span = "".join(csegs)
            else:
                warning("%s:%d: No transformation for tag '%s' and format '%s'."
                        % (path, lno, tag, fmt))
        elif tag == "numid":
            has_any_kuit_tag = True
            # What to do with numid tag.
            # 0 - Simply remove numid tag, with a warning to manually convert
            #     associated argument into digit string.
            # 1 - Modify all placeholders in the text wrapped with numid
            #     to %I<N> form, which indicates numeric identifier formatting.
            numid_tag_res = 0
            if numid_tag_res == 0:
                if not path.endswith((".po", ".pot")):
                    warning("%s:%d: A '%s' tag has been removed, do something "
                            "manually with any of its associated argument "
                            "(e.g. wrap it in QString::number())."
                            % (path, lno, tag))
                res_span = res_etext
            elif numid_tag_res == 1:
                nisegs = []
                p1b = 0
                while True:
                    p2b = res_etext.find("%", p1b)
                    if p2b < 0:
                        nisegs.append(res_etext[p1b:])
                        break
                    nisegs.append(res_etext[p1b:p2b])
                    if res_etext[p2b + 1:p2b + 2].isdigit():
                        p3b = p2b + 1
                        while p3b < len(res_etext) and res_etext[p3b].isdigit():
                            p3b += 1
                        nisegs.append("%I" + res_etext[p2b + 1:p3b])
                        p1b = p3b
                    else:
                        nisegs.append("%")
                        p1b += 1
                res_span = "".join(nisegs)
            else:
                raise StandardError("Unknown '%s' tag resolution choice '%d'."
                                    % ("numid", numid_tag_res))
        elif tag in _html_tags:
            has_any_html_tag = True
            if tag.lower() in ("qt", "html"):
                has_top_tag = True
            if not in_place:
                res_span = "<%s%s>%s</%s>" % (tag, attrstr, res_etext, tag)
        segs.append(res_span)
        p1 = p3
    res_text = "".join(segs)
    return res_text, has_any_kuit_tag, has_any_html_tag, has_top_tag


_entity_rx = re.compile(r"&([a-z]+|#[0-9]+|#x[0-9a-fA-F]+);", re.U | re.S)

_xml_entities = {
    "lt": "<",
    "gt": ">",
    "amp": "&",
    "apos": "'",
    "quot": "\"",
}

def resolve_entities (text, path, lno):

    any_entity_resolved = False
    segs = []
    p1 = 0
    while True:
        m = _entity_rx.search(text, p1)
        if not m:
            segs.append(text[p1:])
            break
        p2, p3 = m.span()
        segs.append(text[p1:p2])
        span = text[p2:p3]
        ent = m.group(1)
        if ent.startswith("#"): # numeric character
            try:
                if ent[1] == "x":
                    c = unichr(int(ent[2:], 16))
                else:
                    c = unichr(int(ent[1:], 10))
            except:
                warning("%s:%d: Invalid numeric XML entity '%s'."
                        % (path, lno, ent))
            segs.append(c)
            any_entity_resolved = True
        elif ent in _xml_entities:
            segs.append(_xml_entities[ent])
            any_entity_resolved = True
        else:
            # Don't warn, may be some HTML entity.
            segs.append(span)
        p1 = p3
    res_text = "".join(segs)
    return res_text, any_entity_resolved


_cmarker_rx = re.compile(r"@(\w+):?(\w+)?/?(\w+)?", re.U | re.S)

def format_from_cmarker (ctxt, quote):

    fmt = None
    res_ctxt = ctxt
    has_cmarker = False
    if ctxt is not None:
        p1 = 0
        if quote:
            p1 = ctxt.find(quote) + len(quote)
        m = _cmarker_rx.match(ctxt, p1)
        if m:
            has_cmarker = True
            role, cue, fmt = m.groups()
            if fmt: # explicit format modifier
                p2 = ctxt.find("/", p1)
                res_ctxt = ctxt[:p2] + ctxt[p2 + 1 + len(fmt):]
            else: # format by role and cue
                if role and cue:
                    fmt = _cmarker_to_format.get("@%s:%s" % (role, cue))
                if not fmt: # format by role alone
                    fmt = _cmarker_to_format.get("@%s" % role)
    return fmt, res_ctxt, has_cmarker


_opentag_rx = re.compile(r"<\s*(\w+)[^>]*>", re.U | re.S)

def format_from_tags (text, quote):

    fmt = None
    for tag in _opentag_rx.findall(text):
        if tag in _html_tags:
            fmt = "rich"
            break
    return fmt


def escape_c (text, quote):

    text = text.replace("\\", "\\\\") # must be first
    if quote:
        text = text.replace(quote, "\\" + quote)
    text = text.replace("\t", "\\t")
    text = text.replace("\n", "\\n")
    return text


def join_quoted (s, quote, invert=False, strip=False):

    segs1 = []
    segs2 = []
    p1 = 0
    l = len(s)
    lq = len(quote)
    while True:
        p2 = find_esc(s, quote, "\\", p1)
        if p2 < 0:
            segs2.append(s[p1:])
            break
        segs2.append(s[p1:p2])
        p2 += len(quote)
        p3 = find_skip_esc(s, quote, "\\", p2)
        if p3 < 0:
            raise StandardError(
                "Malformed concatenated string literal '%s'." % s)
        segs1.append(s[p2:p3])
        p1 = p3 + len(quote)
    js1 = "".join(segs1)
    js2 = "".join(segs2)
    js = js1 if not invert else js2
    if not strip:
        js = quote + js + quote
    return js


def find_esc (s, f, e, p=0):

    ls = len(s)
    le = len(e)
    while p < ls:
        if s.startswith(e, p):
            p += le + 1
        elif s.startswith(f, p):
            break
        else:
            p += 1
    if p >= ls:
        p = -1
    return p


_xml_entities_escape_ordered = [
    ("&", "&amp;"), # must be first
    ("<", "&lt;"),
    (">", "&gt;"),
    ("\"", "&quot;"),
    ("'", "&apos;"),
]
_xml_entities_unescape_ordered = [
    tuple(reversed(x)) for x in reversed(_xml_entities_escape_ordered)]

def unescape_xml (es, testnoesc=False):

    s = es
    if testnoesc:
        noesc = set()
    for ent, val in _xml_entities_unescape_ordered:
        if testnoesc:
            p = s.find(val)
            if p >= 0 and not s.startswith(ent, p): # for & -> &amp;
                noesc.add(ent)
        s = s.replace(ent, val)
    if testnoesc:
        return s, noesc
    else:
        return s

def escape_xml (s, noesc=None):

    es = s
    for val, ent in _xml_entities_escape_ordered:
        if not noesc or ent not in noesc:
            es = es.replace(val, ent)
    return es


if __name__ == "__main__":
    main()

