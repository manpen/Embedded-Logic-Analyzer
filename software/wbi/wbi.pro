PROJECT                 = Wishbone Connector
TEMPLATE                = app
VERSION                 = 0.0.1
DESTDIR                 = build
#CONFIG                 += debug
config                  += release
config                  -= console

QMAKE_LFLAGS_CONSOLE	= -Wl,-subsystem,wi

OBJECTS_DIR             = tmp
MOC_DIR                 = tmp
DEPENDDIR               = .
INCLUDEDIR              = .

HEADERS = \
    hardware/LogicAnalyzer.h \
    hardware/WBI.h \
    hardware/WBISerial.h \
    hardware/BitVector.h \
    hardware/DataTrace.h \
    hardware/VCDExport.h \
    gui/SerialPortChooser.h \
    gui/MainWindow.h \
    gui/TriggerButtons.h

SOURCES = main.cpp \
    hardware/WBISerial.cpp \
    hardware/BitVector.cpp \
    hardware/LogicAnalyzer.cpp \
    hardware/DataTrace.cpp \
    hardware/VCDExport.cpp \
    gui/SerialPortChooser.cpp \
    gui/MainWindow.cpp \
    gui/TriggerButtons.cpp

DEFINES                 += QEXTSERIALPORT_LIB

HEADERS += qextserial/qextserialport.h \
    qextserial/qextserialenumerator.h \
    qextserial/qextserialport_global.h

SOURCES += qextserial/qextserialport.cpp

unix:SOURCES += qextserial/posix_qextserialport.cpp
unix:!macx:SOURCES  += qextserial/qextserialenumerator_unix.cpp
macx {
  SOURCES += qextserial/qextserialenumerator_osx.cpp
  LIBS    += -framework IOKit -framework CoreFoundation
}

win32 {
  SOURCES += qextserial/win_qextserialport.cpp  \
    qextserial/qextserialenumerator_win.cpp
  DEFINES          += WINVER=0x0501 # needed for mingw to pull in appropriate dbt business...probably a better way to do this
  LIBS    += -lsetupapi -lqtmain

#  debug:CONFIG += console
}

OTHER_FILES += \
    Doxyfile \
    app-icon.rc

FORMS += \
    gui/SerialPortChooser.ui \
    gui/MainWindow.ui

RESOURCES += \
    icons.qrc

RC_FILE = app-icon.rc
