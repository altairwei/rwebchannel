QT += gui webchannel widgets websockets

CONFIG += warn_on

SOURCES += \
    main.cpp \
    dialog.cpp \
    websockettransport.cpp \
    websocketclientwrapper.cpp

HEADERS += \
    core.h \
    dialog.h \
    websockettransport.h \
    websocketclientwrapper.h

FORMS += \
    dialog.ui

DEFINES += "BUILD_DIR=\"\\\""$$OUT_PWD"\\\"\""

exampleassets.files += \
    index.html
exampleassets.path = $$[QT_INSTALL_EXAMPLES]/webchannel/standalone
include(../exampleassets.pri)

target.path = $$[QT_INSTALL_EXAMPLES]/webchannel/standalone
INSTALLS += target
