    Free Pascal Profiler

Introduction
------------
The Free Pascal Profiler is an effort to create an FPC native profiler,
deployable on all platforms and architectures the Free Pascal compiler
itself supports.

Free Pascal Profiler conists of two tools (fpp and fppview) and some units
that contain the profiling code that needs to be linked in.

FPP
---
You pass the same parameters to FPP as you would to FPP to compile your
project. FPP then scans your source code, and inserts profiling code in
each begin..end block. It also ammends the uses clause to link in the
fpprof unit. Once done, it compiles your project which now includes the
profiling code. After that it restores your units to there original state.

FPP View
--------
When your run your program that contains the profiling code, it generates
a xml file will all the profiling information. FPP View reads this xml file
and generates reports from it. Call counts, timing and call graphs are
supported at the moment.


                    -------  oOo  --------


