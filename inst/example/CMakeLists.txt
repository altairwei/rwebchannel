find_package( Qt5Core CONFIG REQUIRED )
find_package( Qt5Gui CONFIG REQUIRED )
find_package( Qt5Widgets CONFIG REQUIRED )
find_package( Qt5WebSockets CONFIG REQUIRED )
find_package( Qt5WebChannel CONFIG REQUIRED )

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(QT_VERSION ${Qt5_VERSION})
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTOUIC ON)

set(SOURCES 
    main.cpp
    dialog.cpp
    websockettransport.cpp
    websocketclientwrapper.cpp
)

set(HEADERS 
    core.h
    dialog.h
    websockettransport.h
    websocketclientwrapper.h
)

set(FORMS dialog.ui)

set(ASSETS index.html qwebchannel.js)

qt5_wrap_ui(${FORMS})

include_directories(
    ${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}
)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

add_executable(webchannel_example ${HEADERS} ${SOURCES})

target_link_libraries( webchannel_example
    Qt5::Core
    Qt5::Gui
    Qt5::Widgets
    Qt5::WebSockets
    Qt5::WebChannel
)

foreach(_file ${ASSETS})
    add_custom_command(TARGET webchannel_example POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/inst/example/${_file} ${CMAKE_BINARY_DIR}/bin)
endforeach()

install(FILES ${ASSETS} DESTINATION bin)