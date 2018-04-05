.. _Format_of_the_Report_File:

*************************
Format of the Report File
*************************

.. index:: Format of the Report File

The *gnatcheck* tool outputs on :file:`stderr` all messages concerning
rule violations except if running in quiet mode.  By default it also creates a
text file that contains the complete report of the last gnatcheck run, this file
is named :file:`gnatcheck.out`. A user can specify generation of
the XML version of the report file (its default name is :file:`gnatcheck.xml`)
If *gnatcheck* is called with a project
file, the report file is located in the object directory defined by the project
file (or in the directory where the argument project file is located if no
object directory is defined), if ``--subdirs`` option is specified, the
file is placed in the subrirectory of this directory specified by this option.
Otherwise it is located in the
current directory; the ``-o`` or ``-ox`` option can be used to
change the name and/or location of the text or XML report file.
This text report contains:


* general details of the *gnatcheck* run: date and time of the run,
  the version of the tool that has generated this report, full parameters
  of the  *gnatcheck* invocation, reference to the list of checked
  sources and applied rules (coding standard);
* summary of the run (number of checked sources and detected violations);
* list of exempted coding standard violations;
* list of non-exempted coding standard violations;
* list of problems in the definition of exemption sections;
* list of language violations (compile-time errors) detected in processed sources;

The references to the list of checked sources and applied rules are
references to the text files that contain the corresponding information.
These files could be either files supplied as *gnatcheck* parameters or
files created by *gnatcheck*; in the latter case
these files are located in the same directory as the report file.

The content of the XML report is similar to the text report except that
it explores the set of files processed by gnatcheck and the coding standard
used for checking these files.
