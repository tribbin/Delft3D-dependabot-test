
# Gather source files
set(source_files	src/coupled.f90
			src/determine_densprof.f90
			src/nfl_data.f90
			src/read_cosumo_file.f90
			src/read_nf2ff_files.f90
			src/write_ff2nf_files.f90
			)

# Define library
set(library_name csumo_nfio)
add_library(${library_name} STATIC ${source_files})

set(library_dependencies    ec_module)


target_link_libraries(${library_name} PRIVATE ${library_dependencies})

# Set dependencies on linux
if(UNIX)

    message(STATUS "Setting linker properties in Linux")
    #set_property(TARGET ${library_name} PROPERTY LINKER_LANGUAGE Fortran)

endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_files})
set_target_properties (${library_name} PROPERTIES FOLDER tools_gpl/csumo_nfio)

