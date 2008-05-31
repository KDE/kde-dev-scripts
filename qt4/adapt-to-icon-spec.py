#!/usr/bin/python

# Written by Luca Gugelmann <lucag@student.ethz.ch> 
# This code is in the public domain.

import os,os.path
import re,sys,tty,termios
import string

# tuple of ints for required python version, will be matched against sys.version_info
# python 2.4 is fine, 2.3 _should_ work too, but is untested, so 2.4 is required
required_python_version = (2, 4)

actions = [ #(new, old)
  ('application-exit', 'exit'),
  ('arrow-down', '1downarrow'),
  ('arrow-down-double', '2downarrow'),
  ('arrow-left', '1leftarrow'),
  ('arrow-left-double', '2leftarrow'),
  ('arrow-right', '1rightarrow'),
  ('arrow-right-double', '2rightarrow'),
  ('arrow-up', '1uparrow'),
  ('arrow-up-double', '2uparrow'),
  ('bookmark-new', 'bookmark_add'),
  ('bookmark-new-list', 'bookmarks_list_add'),
  ('calendar-today', 'today'),
  ('character-set', 'charset'),
  ('color-picker', 'colorpicker'),
  ('configure-shortcuts', 'configure_shortcuts'),
  ('configure-toolbars', 'configure_toolbars'),
  ('user-identity', 'identity'),
  ('dialog-ok-apply', 'apply'),
  ('dialog-cancel', 'button_cancel'),
  ('dialog-error', 'messagebox_critical'),
  ('dialog-information', 'messagebox_info'),
  ('dialog-ok', 'button_ok'),
  ('dialog-warning', 'messagebox_warning'),
  ('document-export', 'fileexport'),
  ('document-import', 'fileimport'),
  ('document-new', 'filenew'),
  ('document-open', 'fileopen'),
  ('document-print', 'fileprint'),
  ('document-print-preview', 'filequickprint'),
  ('document-revert', 'revert'),
  ('document-revert', 'file-revert'),
  ('document-save', 'filesave'),
  ('document-save-as', 'filesaveas'),
  ('edit-clear', 'editclear'),
  ('edit-copy', 'editcopy'),
  ('edit-cut', 'editcut'),
  ('edit-delete', 'editdelete'),
  ('edit-delete-shred', 'editshred'),
  ('edit-find', 'find'),
  ('edit-paste', 'editpaste'),
  ('edit-redo', 'redo'),
  ('edit-trash', 'edittrash'),
  ('edit-undo', 'undo'),
  ('system-search', 'filefind'),
  ('system-search', 'file-find'),
  ('file-import', 'fileimport'),
  ('file-revert', 'revert'),
  ('fileview-detailed', 'view_detailed'),
  ('fileview-multicolumn', 'view_multicolumn'),
  ('fileview-text', 'view_text'),
  ('find-next', 'next'),
  ('find-previous', 'previous'),
  ('folder-new', 'folder_new'),
  ('folder-bookmarks', 'bookmark_folder'),
  ('folder-bookmarks', 'bookmark-folder'),
  ('format-indent-less', 'unindent'),
  ('format-indent-more', 'indent'),
  ('format-justify-fill', 'text_block'),
  ('format-justify-right', 'rightjust'),
  ('format-text-bold', 'text_bold'),
  ('format-text-italic', 'text_italic'),
  ('format-text-strikethrough', 'text_strike'),
  ('format-text-underline', 'text_under'),
  ('frame-edit', 'frame_edit'),
  ('get-hot-new-stuff', 'knewstuff'),
  ('go-bottom', 'bottom'),
  ('go-down', 'down'),
  ('go-first', 'start'),
  ('go-home', 'gohome'),
  ('go-last', 'finish'),
  ('go-next', 'forward'),
  ('go-previous', 'back'),
  ('go-top', 'top'),
  ('go-up', 'up'),
  ('go-jump', 'goto'),
  ('go-jump', 'goto-page'),
  ('help-contents', 'contents'),
  ('help-contents', 'help'),
  ('help-contextual', 'contexthelp'),
  ('help-contextual', 'help-whatsthis'),
  ('history-clear', 'history_clear'),
  ('images-display', 'images_display'),
  ('insert-object', 'frame_text'),
  ('list-add', 'add'),
  ('list-remove', 'remove'),
  ('list-add-user', 'add_user'),
  ('list-add-user', 'add-user'),
  ('list-remove-user', 'delete_user'),
  ('list-remove-user', 'delete-user'),
  ('mail', 'mail_generic'),
  ('mail-forward', 'mail_forward'),
  ('mail-mark-notjunk', 'mail_ham'),
  ('mail-reply-all', 'mail_replyall'),
  ('mail-reply-sender', 'mail_reply'),
  ('mail-send', 'mail_send'),
  ('media-eject', 'player_eject'),
  ('media-playback-pause', 'player_pause'),
  ('media-playback-start', 'player_play'),
  ('media-playback-stop', 'player_stop'),
  ('media-record', 'player_record'),
  ('media-seek-backward', 'player_rew'),
  ('media-seek-forward', 'player_fwd'),
  ('media-skip-backward', 'player_start'),
  ('media-skip-forward', 'player_end'),
  ('kde', 'about_kde'),
  ('kdeprint-addprinter', 'kdeprint_addprinter'),
  ('kdeprint-printer-infos', 'kdeprint_printer_infos'),
  ('kdeprint-testprinter', 'kdeprint_testprinter'),
  ('network-connect', 'connect_established'),
  ('network-disconnect', 'connect_no'),
  ('none', 'kdeprint_configmgr'),
  ('none', 'kdeprint_configsrv'),
  ('none', 'kdeprint_defaulthard'),
  ('none', 'kdeprint_defaultsoft'),
  ('none', 'kdeprint_printstate'),
  ('none', 'kdeprint_report'),
  ('none', 'kdeprint_restartsrv'),
  ('none', 'kdeprint_uploadsmb'),
  ('none', 'view_change'),
  ('object-rotate-left', 'rotate_ccw'),
  ('object-rotate-right', 'rotate_cw'),
  ('print-frame', 'frameprint'),
  ('printer-queue-state', 'kdeprint_queuestate'),
  ('process-stop', 'stop'),
  ('run-build', 'make_kdevelop'),
  ('run-build-file', 'compfile'),
  ('search-filter', 'filter'),
  ('show-menu', 'showmenu'),
  ('system-lock-screen', 'lock'),
  ('system-run', 'run'),
  ('tab-new', 'tab_new'),
  ('tab-close', 'tab_remove'),
  ('text-completion', 'completion'),
  ('thumbnail-show', 'thumbnail'),
  ('tools-check-spelling', 'spellcheck'),
  ('view-fullscreen', 'window_fullscreen'),
  ('view-refresh', 'reload'),
  ('view-restore', 'window_nofullscreen'),
  ('window-close', 'fileclose'),
  ('window-new', 'window_new'),
  ('zoom-fit-best', 'viewmagfit'),
  ('zoom-in', 'viewmag+'),
  ('zoom-original', 'viewmag'),
  ('zoom-out', 'viewmag-'),
  ('dialog-close', 'fileclose'),
  ('document-open-recent', 'fileopen'),
  ('document-properties', 'info'),
  ('document-revert', 'reload'),
  ('mail-message-new', 'mail_send'),
  ('system-log-out', 'exit'),
  ('system-search', 'kfind'),
  ('window-close', 'fileclose'),
  ('browser-go', 'key_enter'),
  ('folder-open', 'folder_open'),
  ('kdeprint-addpseudo', 'kdeprint_addpseudo')
]

places = [
  ('network-workgroup', 'network_local'),
  ('user-home', 'folder_home'),
  ('file-broken', 'file_broken'),
  ('network-wired', 'network'),
  ('user-trash', 'trashcan_empty'),
  ('user-trash-full', 'trashcan_full')
]

iconnames = []
iconnames += actions
iconnames += places

# Returns true if python is too old.
# Pass version in a tuple of ints, e.g. (2, 4, 2)
# version check <- for greppers
def python_too_old( required_version ):
	py_version = sys.version_info[:3] #only compare maj,min,tiny
	if len(required_version) > 3:
		required_version = required_version[:3]
	for n in xrange(len(required_version)):
		if py_version[n] < required_version[n]:
			return True
	return False

def regexp_clean_name( name ):
	for c in "+.?^$*()":
		name = name.replace( c, "\\" + c )
	return name

# the representation of icon names as tuples is quite useful for editing,
# but useless for searching...
new_names = [ n[0] for n in iconnames ]
new_names_clean = [ regexp_clean_name(n) for n in new_names ]
old_names = [ n[1] for n in iconnames ]
old_names_clean = [ regexp_clean_name(n) for n in old_names ]

# The idea behind the giant-regexp is to have the looping done in c code, not
# python. This is a bit faster than looping through the single regexps.
giant_match_all_regexp = re.compile( '"' + '"|"'.join(old_names_clean) + '"')
old_names_regexps = [ re.compile( '"%s"' % n ) for n in old_names_clean ]

# This function return True when it thinks that it has found a sure candidate
# for replacement
def strongPositive( line, old_name, a, b ):
	old_name = regexp_clean_name(old_name)
	regexps = [ 'KIcon\( *("%s") *\)' % old_name,
				'KIcon\( *QLatin1String\( *("%s")' % old_name,
				'SmallIcon\( *("%s") *\)' % old_name,
				'SmallIconSet\( *("%s") *\)' % old_name,
				'BarIconSet\( *("%s") *\)' % old_name, 
				'KGuiItem\( *i18n\( *".*" *\) *, *("%s")' % old_name,
				'loadIcon\( *("%s")' % old_name,
				'setIcon\( *("%s")' % old_name, 
				'DesktopIcon\( *("%s")' % old_name,
				'BarIcon\( *("%s")' % old_name,
				'Plasma::menuIconSet\( *("%s")' % old_name
				]

	for r in regexps:
		s = re.search( r, line )
		if s:
			# The boundary check is done because one could have a dodgy line such as:
			#   mySpamFunction( "spam", KIcon("spam"))
			# where the first "spam" is the one that triggered the match and is not
			# a candidate for replacement. The line will pop-up again for the
			# second "spam".
			if s.start(1) == a and s.end(1) == b:
				return True
	return False

# This function return True when it really thinks that no substitution should
# take place
def strongNegative( line, old_name, a, b ):
	old_name = regexp_clean_name(old_name)
	regexps = [ 'actionCollection\(\)->addAction\( *("%s")' % old_name,
				'addAction\( *("%s")' % old_name ]

	for r in regexps:
		s = re.search( r, line )
		if s:
			if s.start(1) == a and s.end(1) == b:
				return True
	return False

def get_context_bounds( string, pos, before = 0, after = 0 ):
	a = string.rfind( "\n", 0, pos) + 1
	b = string.find( "\n", pos)
	if b == -1: b = len(string) -1
	for n in xrange(before):
		if a > 0:
			a = string.rfind( "\n", 0, a - 2) + 1
	for n in xrange(after):
		b = string.find( "\n", b+1)
		if b == -1:
			b = len(string) -1
			break
	return a,b

# user input without having to press enter as it would be with
# sys.stdin.read(1)
def get_user_input():
	fd = sys.stdin.fileno()
	old_settings = termios.tcgetattr(fd)
	try:
		tty.setraw(sys.stdin.fileno())
		c = sys.stdin.read(1)
	finally:
		termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
        return c

def print_context( filename, string, match, new_name, old_name, lines = 0):
	print "--- " + filename + " ---"
	a,b = get_context_bounds(string, match.start())
	if lines > 0:
		c,d = get_context_bounds(string, max(0, a-2), before = lines-1)
		if c < a:
			for line in string[c:d].split('\n'):
				print '| ' + line.expandtabs(4)
	print '> ' + string[a:b].expandtabs(4)
	print ' '*(len(string[a:match.start()].expandtabs(4))+2) + "^"*(match.end() - match.start())
	if lines > 0:
		c,d = get_context_bounds(string, b+1, after = lines-1)
		if d > b:
			for line in string[c:d].split('\n'):
				print '| ' + line.expandtabs(4)
	print '-'
	print 'replace "%s" with "%s"?' % (old_name, new_name)
	print "press y, [n] or ? for more options ",

# return values: 0 noreplace, continue; 1 replace, continue, 2 noreplace, restart
def ask_user(filename, string, match, new_name, old_name, context = 0):
	while True: 
		print_context(filename, string, match, new_name, old_name, context)
		ret = get_user_input()
		print ret
		if ret == 'q' or ret == '\x03': #ctrl-c
			sys.exit(0)
		if ret == 'y':
			return 1
		if ret == 'm': #more context
			context += 1
			continue
		if ret == 'l': #less context
			context = max(0,context-1)
			continue
		if ret == 'e': #start editor
			editor = ''
			if os.environ.has_key("EDITOR"):
				editor = os.environ["EDITOR"]
			elif os.environ.has_key("VISUAL"):
				editor = os.environ["VISUAL"]
			else:
				print
				print "You need to specify your editor via the"
				print "EDITOR or VISUAL environment variables."
				print
				print "press any key to continue."
				get_user_input()
				continue
			mtime = os.stat(filename).st_mtime
			os.system("%s %s" % (editor, filename))
			if os.stat(filename).st_mtime != mtime:
				# since the file has changed, it needs to be reparsed
				return 2
			else:
				continue
		if ret == '?':
			print
			print "q: quit."
			print "m: more context."
			print "l: less context."
			print "y: perform the suggested substitution in-place."
			print "n: do not substitute, continue running."
			print "e: edit the current file in your favourite editor (as in $EDITOR)."
			print "?: this help."
			print
			print "press the any key to continue (or just any key)."
			get_user_input()
			continue
		if ret == 'n' or ret == '\r' or ret == '\n':
			return 0

def rename_icons( filename ):
	f = open(filename, 'r')
	s = f.read()
	f.close()
	m = giant_match_all_regexp.search( s )
	file_changed = False
	index = 0
	for r in old_names_regexps:
		m = r.search( s )
		while ( m != None ):
			offset = 0
			old_name = old_names[index]
			new_name = new_names[index]
			a,b = get_context_bounds( s, m.start() )
			line = s[ a:b ]
			if strongPositive(line, old_name, m.start() - a, m.end() - a):
				s = s[:m.start()] + '"' + new_name + '"' + s[m.end():]
				offset = len(new_name) - len(old_name)
				file_changed = True
			elif not strongNegative( line, old_name, m.start() - a, m.end() - a):
				ret = ask_user(filename, s, m, new_name, old_name)
				if ret == 1:
					s = s[:m.start()] + '"' + new_name + '"' + s[m.end():]					
					offset = len(new_name) - len(old_name)
					file_changed = True
				if ret == 2:
					rename_icons(filename)
					return
			m = r.search(s, m.end() + offset)
		index += 1
	if file_changed:
		f = open(filename, 'w')
		f.write(s)
		f.close()

def walk_path( path ):
	for root, dirs, files in os.walk(path):
		if '.svn' in dirs:
			dirs.remove( '.svn' )
		for f in files:
			for ending in ('.cpp', '.h', '.cc' ): # no .endswith with tuples in python 2.4...
				if f.endswith( ending ):
					rename_icons( os.path.join( root, f ) )
					break

if __name__ == "__main__":
# Ensure version is high enough
	if python_too_old(required_python_version):
		print "This script requires Python", '.'.join(map(str, required_python_version))
		sys.exit(1)

	if len(sys.argv) > 1:
		for f in sys.argv[1:]:
			if not os.path.isfile(f):
				break
			found = False
			for ending in ('.cpp', '.h', '.cc' ):
				if f.endswith(ending): #python 2.4 doesen't have the possibility to pass a tuple here
					rename_icons( f )
					found = True
					break
			if not found:
				# trust me, you WILL end up running this script on itself,
				# thus possibly destroying it.
				print "I cowardly refuse to run on", f
	else:
		walk_path(os.getcwd())

# vim: set noet sw=8 ts=8:
