" To use this file, add this line to your ~/.vimrc:, w/o the dquote
" source /path/to/kde/sources/kdesdk/scripts/kde-devel-vim.vim

" Don't include these in filename completions
set suffixes+=.lo,.o,.moc,.la,.closure,.loT

" Search for headers here
set path=.,/usr/include,/usr/local/include,
if $QTDIR != ''
	let &path = &path . $QTDIR . '/include/,'
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

" Expand #i to #include <.h> or #include ".h". The latter is chosen
" if the character typed after #i is a dquote
iab #i <C-R>=SmartInclude()<CR>

" Insert a stripped down CVS diff
iab DIFF <Esc>:call RunDiff()<CR>

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
		if filereadable( fn )
			execute( "edit ".fn )
		endif
	elseif fn =~ '\.h$'
		let fn = substitute( fn, '\.h$', '.cpp', '' )
		if filereadable( fn )
			execute( "edit ".fn )
			return
		endif
		let fn = substitute( fn, '\.cpp$', '.cc', '' )
		if filereadable( fn )
			execute( "edit ".fn )
			return
		endif
		let fn = substitute( fn, '\.cc$', '.C', '' )
		if filereadable( fn )
			execute( "edit ".fn )
			return
		endif
		let fn = substitute( fn, '\.C$', '.c', '' )
		if filereadable( fn )
			execute( "edit ".fn )
			return
		endif
	endif
endfunction

function! IncludeGuard()
	let guard = substitute( expand( '%' ), '\([^.]*\)\.h', '__\1_h__', '' )
	call append( '.', '#ifndef ' . guard )
	+
	call append( '.', '#define ' . guard )
	call append( '$', '#endif' )
	+
endfunction

function! SmartInclude()
	if nr2char( getchar( 0 ) ) == '"'
		return "#include \".h\"\<Left>\<Left>\<Left>"
	endif
	return "#include <.h>\<Left>\<Left>\<Left>"
endfunction

function! RunDiff()
	echo 'Diffing....'
	read! cvs diff -bB -I \\\#include | egrep -v '(^Index:|^=+$|^RCS file:|^retrieving revision|^diff -u|^[+-]{3})'
endfunction

function CreateChangeLogEntry()
    let currentBuffer = expand( "%" )

    if exists( "$EMAIL" )
        let mail = $EMAIL
    else
        let mail = inputdialog( "Enter Name/Email for Changelog entry: " ) 
	if mail == ""
	    echo "Aborted ChangeLog edit..."
	    return
	endif
	" ### emacs is more clever by storing that information persistently
	" in .emacs 
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

