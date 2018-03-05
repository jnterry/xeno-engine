option(XEN_STRICT_COMPILE "Enables extra compiler warnings and errors" ON)

if(XEN_STRICT_COMPILE)
  #setup warnings + errors
  if(MSVC)
	 # Force to always compile with W4
	 if(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
		string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
	 else()
		set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
	 endif()
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4268")   # ignore C4268 - const static/global object initied with compiler default constructors
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4100")   # ignore C4100 - unused parameter to function
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4201")   # ignore C4100 - using nameless struct inside union

	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /we4146")   # make '-(unsigned_type)' an error, use -((int)(unsigned_type)) instead

	 #enable some warnings that are disabled by default, even at /W4
	 #/Wall has too many, so just enable ones that make sence
	 #see https://msdn.microsoft.com/en-us/library/23k5d385.aspx
	 #set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w44061 /w44062") # missing case in enum switch
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w44365 /w44388") # implicit conversion between unsigned and signed
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w34738")         # ran out of float registers, putting in memory
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w14311")         # pointer truncation (assigned pointer to int type thats too small)
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w44296")         # if comparision always evaluates to false
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w44302")         # conversion from larger to smaller type

  elseif(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
	 set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-long-long -pedantic")
  endif()
endif()
