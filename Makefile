
TARGET = kcvs2pack

all: $(TARGET)

doc: $(TARGET).1 $(TARGET).txt

$(TARGET).1: $(TARGET)
	pod2man -center="K Desktop Environment" $(TARGET) > $(TARGET).1

$(TARGET).txt: $(TARGET)
	pod2text $(TARGET) > $(TARGET).txt

install: doc
	install -m 755 $(TARGET) /usr/local/bin/$(TARGET)
	install -m 644 $(TARGET).1 /usr/local/man/man1/$(TARGET).1
