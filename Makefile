# Demo testing

# Prerequisites for building:
# - CC65 compiled and included in path with sudo make avail
# - ZIP packages installed: sudo apt-get install zip
# - wput command installed: sudo apt-get install wput

# Path variables
EXOMIZER = /home/xahmol/exomizer/src/exomizer

# Hostname of Ultimate II+ target for deployment. Edit for proper IP and usb number
ULTHOST = ftp://192.168.1.19/usb1/dev/
ULTHOST2 = ftp://192.168.1.31/usb1/dev/

# Program name and sources
MAIN = program.prg
SOURCEMAIN = src/main.s
OBJECTS = music.prg screen.prg
PACKED = demo.prg

# CC65 parameters
CC65_TARGET = c128
CC = cl65
LDFLAGSMAIN = -t $(CC65_TARGET) -C main-config.cfg -m $(MAIN).map

# Exomizer parameters
SYSADDRESS = 0x3000
EXOPARAMS = sfx $(SYSADDRESS) -t 128

########################################

.SUFFIXES:
.PHONY: all clean deploy vice
all: $(MAIN) $(BASIC) $(PACKED)
  
$(MAIN): $(SOURCEMAIN)
	$(CC) $(LDFLAGSMAIN) -o $@ $^

$(BASIC): $(SOURCEBASIC)
	$(CC) $(LDFLAGSBASIC) -o $@ $^

$(PACKED): $(MAIN) $(OBJECTS)
	$(EXOMIZER) $(EXOPARAMS) -o $(PACKED) $(BASIC) $(MAIN) $(OBJECTS)

clean:
	$(RM) $(MAIN) $(MAIN).map
	
# To deploy software to UII+ enter make deploy. Obviously C128 needs to powered on with UII+ and USB drive connected.
deploy: $(MAIN)
	wput -u $(PACKED) $(ULTHOST)
	wput -u $(PACKED) $(ULTHOST2)

# To run software in VICE
vice: $(D64)
	x128 $(PACKED)
