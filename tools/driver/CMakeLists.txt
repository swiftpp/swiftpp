add_swift_host_tool(swiftpp
  driver.cpp
  autolink_extract_main.cpp
  modulewrap_main.cpp
  swift_format_main.cpp
  SWIFT_COMPONENT compiler
)
target_link_libraries(swiftpp
                      PRIVATE
                        swiftDriver
                        swiftFrontendTool)
if(HAVE_UNICODE_LIBEDIT)
  target_link_libraries(swiftpp PRIVATE edit)
endif()

swift_create_post_build_symlink(swiftpp
  SOURCE "swiftpp${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swiftppc${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swiftpp
  SOURCE "swiftpp${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-format${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

swift_create_post_build_symlink(swiftpp
  SOURCE "swiftpp${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION "swift-autolink-extract${CMAKE_EXECUTABLE_SUFFIX}"
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

add_swift_tool_symlink(swiftppc swiftpp compiler)
add_swift_tool_symlink(swift-autolink-extract swiftpp autolink-driver)
add_swift_tool_symlink(swift-format swiftpp editor-integration)

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swiftpp clang-headers)
endif()

swift_install_in_component(compiler
    FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swiftppc${CMAKE_EXECUTABLE_SUFFIX}"
    DESTINATION "bin")
swift_install_in_component(autolink-driver
    FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-autolink-extract${CMAKE_EXECUTABLE_SUFFIX}"
    DESTINATION "bin")
swift_install_in_component(editor-integration
    FILES "${SWIFT_RUNTIME_OUTPUT_INTDIR}/swift-format${CMAKE_EXECUTABLE_SUFFIX}"
    DESTINATION "bin")
