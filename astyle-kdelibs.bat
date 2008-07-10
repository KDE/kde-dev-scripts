@echo off 
:: apply kdelibs coding style to all c, cpp and header files in and below the current directory 
::
:: the coding style is defined in http://techbase.kde.org/Policies/Kdelibs_Coding_Style 
::
:: requirements: installed astyle 

FOR /R %%G in (*.c) DO astyle --indent=spaces=4 --brackets=linux --indent-labels --pad=oper --unpad=paren  --one-line=keep-statements --convert-tabs --indent-preprocessor "%%G" 
FOR /R %%G in (*.cpp) DO astyle --indent=spaces=4 --brackets=linux --indent-labels --pad=oper --unpad=paren  --one-line=keep-statements --convert-tabs --indent-preprocessor "%%G" 
FOR /R %%G in (*.h) DO astyle --indent=spaces=4 --brackets=linux --indent-labels --pad=oper --unpad=paren  --one-line=keep-statements --convert-tabs --indent-preprocessor "%%G" 
