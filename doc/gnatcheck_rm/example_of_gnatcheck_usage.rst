.. _Example_of_gnatcheck_Usage:

****************************
Example of *gnatcheck* Usage
****************************

Here is a simple example. Suppose that in the current directory we have a
project file named :file:`gnatcheck_example.gpr` with the following content:


.. code-block:: ada

  project Gnatcheck_Example is

     for Source_Dirs use ("src");
     for Object_Dir use "obj";
     for Main use ("main.adb");

     package Check is
        for Default_Switches ("ada") use ("-rules", "-from=coding_standard");
     end Check;

  end Gnatcheck_Example;


And the file named :file:`coding_standard` is also located in the current
directory and has the following content:


::

  -----------------------------------------------------
  -- This is a sample gnatcheck coding standard file --
  -----------------------------------------------------

  --  First, turning on rules, that are directly implemented in gnatcheck
  +RAbstract_Type_Declarations
  +RAnonymous_Arrays
  +RLocal_Packages
  +RFloat_Equality_Checks
  +REXIT_Statements_With_No_Loop_Name

  --  Then, activating compiler checks of interest:
  +RStyle_Checks:e
  --  This style check checks if a unit name is present on END keyword that
  --  is the end of the unit declaration


And the subdirectory :file:`src` contains the following Ada sources:

:file:`pack.ads`:


.. code-block:: ada

  package Pack is
     type T is abstract tagged private;
     procedure P (X : T) is abstract;

     package Inner is
        type My_Float is digits 8;
        function Is_Equal (L, R : My_Float) return Boolean;
     end Inner;
  private
     type T is abstract tagged null record;
  end;


:file:`pack.adb`:


.. code-block:: ada

  package body Pack is
     package body Inner is
        function Is_Equal (L, R : My_Float) return Boolean is
        begin
           return L = R;
        end;
     end Inner;
  end Pack;


and :file:`main.adb`


.. code-block:: ada

  with Pack; use Pack;
  procedure Main is

     pragma Annotate
       (gnatcheck, Exempt_On, "Anonymous_Arrays", "this one is fine");
     Float_Array : array (1 .. 10) of Inner.My_Float;
     pragma Annotate (gnatcheck, Exempt_Off, "Anonymous_Arrays");

     Another_Float_Array : array (1 .. 10) of Inner.My_Float;

     use Inner;

     B : Boolean := False;

  begin
     for J in Float_Array'Range loop
        if Is_Equal (Float_Array (J), Another_Float_Array (J)) then
           B := True;
           exit;
        end if;
     end loop;
  end Main;


And suppose we call *gnatcheck* from the current directory using
the project file as the only parameter of the call:


::

     gnatcheck -Pgnatcheck_example.gpr


As a result, *gnatcheck* is called to check all the files from the
project :file:`gnatcheck_example.gpr` using the coding standard defined by
the file :file:`coding_standard`. The *gnatcheck*
report file named :file:`gnatcheck.out` will be created in the ``obj``
directory, and it will have the following content:


::

  RULE CHECKING REPORT

  1. OVERVIEW

  Date and time of execution: 2009.10.28 14:17
  Tool version: GNATCHECK (built with ASIS 2.0.R for GNAT Pro 6.3.0w (20091016))
  Command line:

  gnatcheck -files=... -cargs -gnatec=... -rules -from=coding_standard

  Coding standard (applied rules):
     Abstract_Type_Declarations
     Anonymous_Arrays
     EXIT_Statements_With_No_Loop_Name
     Float_Equality_Checks
     Local_Packages

     Compiler style checks: -gnatye

  Number of coding standard violations: 6
  Number of exempted coding standard violations: 1

  2. DETECTED RULE VIOLATIONS

  2.1. NON-EXEMPTED VIOLATIONS

  Source files with non-exempted violations
     pack.ads
     pack.adb
     main.adb

  List of violations grouped by files, and ordered by increasing source location:

  pack.ads:2:4: declaration of abstract type
  pack.ads:5:4: declaration of local package
  pack.ads:10:30: declaration of abstract type
  pack.ads:11:1: (style) "end Pack" required
  pack.adb:5:19: use of equality operation for float values
  pack.adb:6:7: (style) "end Is_Equal" required
  main.adb:9:26: anonymous array type
  main.adb:19:10: exit statement with no loop name

  2.2. EXEMPTED VIOLATIONS

  Source files with exempted violations
     main.adb

  List of violations grouped by files, and ordered by increasing source location:

  main.adb:6:18: anonymous array type
     (this one is fine)

  2.3. SOURCE FILES WITH NO VIOLATION

     No files without violations

  END OF REPORT
