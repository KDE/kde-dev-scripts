
TARGET = cvs2pack

bin_SCRIPTS = $(TARGET) cxxmetric cvschanged cvsversion

doc: $(TARGET).1 $(TARGET).txt

man1_DATA = $(TARGET).1 cvschanged.1 cvsversion.1
man1dir = $(mandir)/man1

$(TARGET).1: $(TARGET)
	pod2man -center="K Desktop Environment" $(TARGET) > $(TARGET).1

$(TARGET).txt: $(TARGET)
	pod2text $(TARGET) > $(TARGET).txt

cvschanged.1: cvschanged
	pod2man -center="K Desktop Environment" cvschanged > cvschanged.1

cvsversion.1: cvsversion
	pod2man -lax -center="K Desktop Environment" cvsversion > cvsversion.1

