
TARGET = kcvs2pack

bin_SCRIPTS = $(TARGET)

doc: $(TARGET).1 $(TARGET).txt

man1_DATA = $(TARGET).1
man1dir = $(mandir)/man1

$(TARGET).1: $(TARGET)
	pod2man -center="K Desktop Environment" $(TARGET) > $(TARGET).1

$(TARGET).txt: $(TARGET)
	pod2text $(TARGET) > $(TARGET).txt

