" To use this file, add this line to your ~/.vimrc:, w/o the dquote
" source /path/to/kde/sources/kdesdk/scripts/kde-devel-vim.vim
"
" For CreateChangeLogEntry() : If you don't want to re-enter your
" Name/Email in each vim session then make sure to have the viminfo
" option enabled in your ~/.vimrc, with the '!' flag, enabling persistent
" storage of global variables. Something along the line of
" set   viminfo=%,!,'50,\"100,:100,n~/.viminfo
" should do the trick.

" Don't include these in filename completions
set suffixes+=.lo,.o,.moc,.la,.closure,.loT

" Search for headers here
set path=.,/usr/include,/usr/local/include,
if $QTDIR != ''
	let &path = &path . $QTDIR . '/include/,'
endif
if $KDEDIR != ''
	let &path = &path . $KDEDIR . '/include/,'
endif
if $KDEDIRS != ''
	let &path = &path . substitute( $KDEDIRS, '\(:\|$\)', '/include,', 'g' )
endif
set path+=,

" Use makeobj to build
set mp=makeobj

" Insert tab character in whitespace-only lines, complete otherwise
inoremap <Tab> <C-R>=SmartTab()<CR>

" Insert a space after ( or [ and before ] or ) unless preceded by a matching
" paren/bracket or space or inside a string or comment. Comments are only
" recognized as such if they start on the current line :-(
inoremap ( <C-R>=SmartParens( '(' )<CR>
inoremap [ <C-R>=SmartParens( '[' )<CR>
inoremap ] <C-R>=SmartParens( ']', '[' )<CR>
inoremap ) <C-R>=SmartParens( ')', '(' )<CR>

" Switch between header and implementation files on ,h
nmap <silent> ,h :call SwitchHeaderImpl()<CR>

" Comment selected lines on ,c in visual mode
vmap ,c :s,^,//X ,<CR>
" Uncomment selected lines on ,u in visual mode
vmap ,u :s,^//X ,,<CR>

" Insert an include guard based on the file name on ,i
nmap ,i :call IncludeGuard()<CR>o

" Insert simple debug statements into each method
nmap ,d :call InsertMethodTracer()<CR>

" Expand #i to #include <.h> or #include ".h". The latter is chosen
" if the character typed after #i is a dquote
" If the character is > #include <> is inserted (standard C++ headers w/o .h)
iab #i <C-R>=SmartInclude()<CR>

" Insert a stripped down CVS diff
iab DIFF <Esc>:call RunDiff()<CR>

" mark 'misplaced' tab characters
set listchars=tab:·\ ,trail:·
set list

set incsearch

function! SmartTab()
	if strpart( getline( '.' ), 0, col( '.' ) - 1 ) =~ '^\s*$'
		return "\<Tab>"
	else
		return "\<C-N>"
	endif
endfunction

function! SmartParens( char, ... )
	if ! ( &syntax =~ '^\(c\|cpp\|java\)$' )
		return a:char
	endif
	let s = strpart( getline( '.' ), 0, col( '.' ) - 1 )
	if s =~ '//'
		return a:char
	endif
	let s = substitute( s, '/\*\([^*]\|\*\@!/\)*\*/', '', 'g' )
	let s = substitute( s, "'[^']*'", '', 'g' )
	let s = substitute( s, '"\(\\"\|[^"]\)*"', '', 'g' )
	if s =~ "\\([\"']\\|/\\*\\)"
		return a:char
	endif
	if a:0 > 0
		if strpart( getline( '.' ), col( '.' ) - 3, 2 ) == a:1 . ' '
			return "\<BS>" . a:char
		endif
		if strpart( getline( '.' ), col( '.' ) - 2, 1 ) == ' '
			return a:char
		endif
		return ' ' . a:char
	endif
	return a:char . ' '
endfunction

function! SwitchHeaderImpl()
	let fn = expand( '%' )
	if fn =~ '\.\(cpp\|cc\|C\|c\)$'
		let fn = substitute( fn, '\.\(cpp\|cc\|C\|c\)$', '.h', '' )
		execute( "edit ".fn )
	elseif fn =~ '\.h$'
		let fn = substitute( fn, '\.h$', '.cpp', '' )
		if filereadable( fn )
			execute( "edit ".fn )
			return
		endif
		let fn = substitute( fn, '\.cpp$', '.C', '' )
		if filereadable( fn )
			execute( "edit ".fn )
			return
		endif
		let fn = substitute( fn, '\.C$', '.c', '' )
		if filereadable( fn )
			execute( "edit ".fn )
			return
		endif
		let tmp = substitute( fn, '\.c$', '.cc', '' )
		execute( "edit ".tmp )
	endif
endfunction

function! IncludeGuard()
	let guard = toupper( substitute( expand( '%' ), '\([^.]*\)\.h', '\1_h', '' ) )
	call append( '^', '#define ' . guard )
	+
	call append( '^', '#ifndef ' . guard )
	call append( '$', '#endif // ' . guard )
	+
endfunction

function! SmartInclude()
	let next = nr2char( getchar( 0 ) )
	if next == '"'
		return "#include \".h\"\<Left>\<Left>\<Left>"
	endif
	if next == '>'
		return "#include <>\<Left>"
	endif
	return "#include <.h>\<Left>\<Left>\<Left>"
endfunction

function! RunDiff()
	echo 'Diffing....'
	read! cvs diff -bB -I \\\#include | egrep -v '(^Index:|^=+$|^RCS file:|^retrieving revision|^diff -u|^[+-]{3})'
endfunction

function! CreateChangeLogEntry()
    let currentBuffer = expand( "%" )

    if exists( "g:EMAIL" )
        let mail = g:EMAIL
    elseif exists( "$EMAIL" )
        let mail = $EMAIL
    else
        let mail = inputdialog( "Enter Name/Email for Changelog entry: " ) 
	if mail == ""
	    echo "Aborted ChangeLog edit..."
	    return
	endif
	let g:EMAIL = mail
    endif

    if bufname( "ChangeLog" ) != "" && bufwinnr( bufname( "ChangeLog" ) ) != -1
	execute bufwinnr( bufname( "ChangeLog" ) ) . " wincmd w"
    else
        execute "split ChangeLog"
    endif

    let lastEntry = getline( nextnonblank( 1 ) )
    let newEntry = strftime("%Y-%m-%d") . "  " . mail

    if lastEntry != newEntry 
        call append( 0, "" )
        call append( 0, "" )
        call append( 0, newEntry )
    endif

    " like emacs, prepend the current buffer name to the entry. but unlike
    " emacs I have no idea how to figure out the current function name :(
    " (Simon)
    if currentBuffer != ""
        let newLine = "\t* " . currentBuffer . ": "
    else
        let newLine = "\t* "
    endif

    call append( 2, newLine )

    execute "normal 3G$"
endfunction

function! AddQtSyntax()
	if expand( "<amatch>" ) == "cpp"
		syn keyword qtKeywords     signals slots emit
		syn keyword qtMacros       Q_OBJECT Q_WIDGET Q_PROPERTY Q_ENUMS Q_OVERRIDE Q_CLASSINFO Q_SETS SIGNAL SLOT
		syn keyword qtCast         qt_cast
		syn keyword kdeKeywords    k_dcop k_dcop_signals
		syn keyword kdeMacros      K_DCOP ASYNC

		hi def link qtKeywords          Statement
		hi def link qtMacros            Type
		hi def link qtCast              Statement
		hi def link kdeKeywords         Statement
		hi def link kdeMacros           Type
	endif
endfunction

function! InsertMethodTracer()
	:normal [[kf(yBjokdDebug() << ""()" << endl;
endfunction

autocmd Syntax * call AddQtSyntax()

