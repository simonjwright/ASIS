.. _Implementation-Specific_Features_and_Implementation_Permissions:

***************************************************************
Implementation-Specific Features and Implementation Permissions
***************************************************************

.. index:: Implementation-specific features
.. index:: Implementation permissions

ASIS permits four kinds of implementation-specific behavior.

.. index:: Asis.Implementation.Initialize procedure
.. index:: Asis.Implementation.Finalize procedure
.. index:: Asis.Ada_Environments.Associate procedure

First, ASIS subprograms that define an interface between an ASIS
implementation and the underlying Ada implementation have
implementation-specific parameters. There are three such queries ---
``Asis.Implementation.Initialize``,
``Asis.Implementation.Finalize`` and
``Asis.Ada_Environments.Associate``.

Each has a string parameter
named ``Parameters`` with an implementation-specific meaning. The meaning
of the ``Parameters`` string in ASIS-for-GNAT is discussed in
:ref:`Interacting_with_the_Underlying_Ada_Implementation`.

.. index:: Asis.Implementation.Permissions package

Second, in some areas the ASIS standard explicitly grants the
implementation permission to provide restricted functionality;
generally this allows omitting features that could present
considerable implementation difficulty.
Such permissions usually affect more than one ASIS query.
The ASIS package ``Asis.Implementation.Permissions``
contains boolean
queries identifying the choices made by a given ASIS implementation.
The ASIS-for-GNAT approach to these implementation permissions is discussed in
:ref:`Implementation_Permissions`.

Third, the ASIS standard defines specific implementation permissions
for some queries.
Also, the result of a query may be implementation specific because of the
nature of the query.
See
:ref:`ASIS_Queries_Having_Specific_Implementation_Permissions_or_Implementation-Specific_Results`.

Finally, ASIS-for-GNAT provides special ``Context`` manipulation mechanisms
that supplement those defined in the ASIS standard.
These additional ``Context`` modes may be useful for
some ASIS applications.

.. _Interacting_with_the_Underlying_Ada_Implementation:

Interacting with the Underlying Ada Implementation
==================================================

This section describes how to use the ``Parameters`` string to
pass implementation-specific information to several ASIS subprograms.

.. _Format_of_the_`Parameters`_String:

Format of the ``Parameters`` String
-----------------------------------

.. index:: Parameters string format
.. index:: Asis.Implementation.Initialize procedure
.. index:: Asis.Implementation.Finalize procedure
.. index:: Asis.Ada_Environments.Associate procedure

A ``Parameters`` string is passed to three ASIS
subprograms: ``Asis.Implementation.Initialize``,
``Asis.Implementation.Finalize``,
and ``Asis.Ada_Environments.Associate``.

The ``Parameters`` string comprises substrings delimited by separators.
The substrings are called *parameters* (with lower-case 'p') below.
A separator is a non-empty string comprising characters from the set
<Space>, <LF>, and <CR>.
There may be 0 or more parameters in a ``Parameters`` string, and there
may be separators before the first and/or after the last parameter.

.. index:: ASIS_Failed exception
.. index:: Parameter_Error error status

Each of the queries ``Asis.Implementation.Initialize``,
``Asis.Implementation.Finalize``, and
``Asis.Ada_Environments.Associate`` has specific rules for the
format of its parameters.
If some parameter is not well-formed,
then either a warning message is generated or else
the ``ASIS_Failed``
exception is raised with the ``Parameter_Error`` status.
The descriptions below explain the situations where
``ASIS_Failed`` is raised.

.. _Parameters_of_Asis.Implementation.Initialize:

Parameters of ``Asis.Implementation.Initialize``
------------------------------------------------

.. index:: Asis.Implementation.Initialize procedure

The allowed parameters for ``Asis.Implementation.Initialize`` are as
follows:


``-d<flag>``
  The specific ASIS-for-GNAT debug flag named ``<flag>`` is set ON

.. index:: Debug flag parameter (to Asis.Implementation.Initialize)

``-dall``
  All the ASIS-for-GNAT debug flags are set ON

``-k``
  Keep going even if an internal implementation error is detected.
  When a non-ASIS exception is raised, it is replaced by
  raising ``ASIS_Failed`` with ``Unhandled_Exception_Error`` status (this
  is the only case when ``Unhandled_Exception_Error`` is set) and the
  ``Diagnosis`` string containing the name and the message from the
  non-ASIS exception originally raised

``-nbb``
  No bug box. Do not output to ``Standard_Error`` the bug box
  containing the description of the internal implementation bug.
  Implies ``-k``

``-sv``
  Set the strong GNAT/ASIS version check when reading the tree files

``-wv``
  Set the weak GNAT/ASIS version check when reading the tree files

``-we``
  All ASIS warnings are treated as errors.

  .. index:: Warning messages
  .. index:: ASIS_Failed exception
  .. index:: Diagnosis string

  When execution reaches the point where the warning would occur,
  the ``ASIS_Failed``
  exception is raised;
  the warning message is the ASIS ``Diagnosis`` string.

``-ws``
  All ASIS warning messages are suppressed.

The ``<flag>`` value for the ``-d`` parameter
may be any lower case letter from ``a`` through ``z`` or any digit
from ``0`` through ``9``, although
not all of the 36 possible flags are implemented.
For more information,
refer to the documentation in the source file :file:`a4g-a_debug.adb`.
See also :ref:`ASIS_Debug_Flags`.

.. index:: Warning messages

If more then one parameter controlling the warning mode
is set in the ``Parameters`` string, all but the last one are ignored.

Parameters of ``Asis.Implementation.Finalize``
----------------------------------------------

.. index:: Asis.Implementation.Finalize procedure

No parameters are allowed for ``Asis.Implementation.Finalize``.

``Asis.Implementation.Finalize`` resets all the general
ASIS-for-GNAT parameters to their default values (that is, all the debug flags
are set OFF, and the warning mode is set to the default warning mode).

Parameters of ``Asis.Ada_Environments.Associate``
-------------------------------------------------

.. index:: Asis.Ada_Environments.Associate procedure

The following parameters are allowed:


``-C1``
  The ``Context`` comprises a single tree file,
  whose name is given as the next parameter in the ``Parameters`` string.

  .. index:: Tree file

``-CN``
  The ``Context`` comprises a set of one or more tree files, whose names are
  given as the next set of parameters in the ``Parameters`` string.

``-CA``
  The ``Context`` comprises all the tree files in the tree search path.

``-FS``
  All the trees considered as making up a given ``Context`` are created
  'on the fly', whether or not the corresponding tree file already exists.
  Once created, a tree file then is reused as long as the ``Context`` remains
  open.

``-FT``
  Only pre-created trees are used; no tree files are created by ASIS.

``-FM``
  Mixed approach: if a needed tree does not exist, an attempt is made to create
  it 'on the fly'.

``-SA``
  Source files for all the ``Compilation_Unit``\ s belonging to the
  ``Context`` (except
  the predefined ``Standard`` package) are considered in the consistency
  check when opening the ``Context``.

  .. index:: Consistency checking

``-SE``
  Only existing source files for all the ``Compilation_Units`` belonging to
  the ``Context`` are considered in the consistency check when opening the
  ``Context``.

``-SN``
  No source files from the underlying file system are taken into account when
  checking the consistency of the set of tree files making up the
  ``Context``.

``-I<dir>``
  Defines the directory in which to search for source files when compiling
  sources to create a tree 'on the fly'.

``--GCC=compiler_name``
  Defines the program to be called to create the tree on the fly

``-gnatec<file>``
  Defines the additional configuration file to be used when calling GNAT to
  create the tree on the fly for ``-FS`` or ``-FM`` Context

``-gnatA``
  Avoid processing :file:`gnat.adc` when calling GNAT to create
  the tree on the fly for ``-FS`` or ``-FM`` Context

``-T<dir>``
  Defines the directory in which to search for a tree file.

``<file_name>``
  Defines the name of a tree file (used in conjunction with ``-C1`` or
  ``-CN``).

.. index:: ASIS_Failed exception

For the ``-I`` and ``-T`` parameters, ``<dir>`` should denote an
existing directory in the underlying file system. The '.' and '..'
notations are allowed, as well as relative or absolute directory names.
If ``<dir>`` does not denote an existing directory, ``ASIS_Failed``
with ``Parameter_Error`` status is raised.

For ASIS ``-FS`` or ``-FM`` Context, Context parameters ``-I``,
``-gnatec`` and ``-gnatA`` are passed to the GNAT call to create
the tree on the fly and these parameters have exactly the same meaning as they
have for GNAT.

A tree file name given by a ``<file_name>`` parameter may or may not
contain directory information.

Any relative directory name or file name containing relative directory
information should start from '.' or '..'.

If a directory or a file name used as a part of some Context parameter contains
space characters, this name should be quoted.

.. index:: Search path

The search path
associated with an ASIS ``Context`` consists of the directories
listed as parameters for the ``Asis.Ada_Environments.Associate`` query, in
the same order as they are included in the actual ``Parameters`` string.
The ASIS source search path consists only of the directories following
``-I``, and the ASIS tree search path consists only of the directories
following ``-T``. If no source (tree) directories are present in the
value of the ``Parameters`` string, then the ASIS source (tree) search path
consists of the current directory only.  Otherwise the current directory is
included in the ASIS search path if and only if it is set explicitly as
``-I.`` or ``-T.`` respectively.

If an ASIS ``Context`` is associated with an ``-FS`` or ``-FM``
option, the ``Context`` source search path is used to locate sources of the
units for which tree files need to be created, and to locate other source
files needed during compilation. For example, if we have:

.. code-block:: ada

  Asis.Ada_Environments.Associate
    (My_Context,
    "My_Context_Name",
    "-CA -FS -I./dir -I.");

then, when processing a call:

.. code-block:: ada

  My_Unit := Asis.Compilation_Units.Library_Unit_Declaration
              ("Foo", My_Context);

ASIS first tries to locate the source file :file:`foo.ads` in :file:`./dir`, and
if this attempt fails, it tries to locate it in the current directory. If
there is no such file in the current directory, ASIS continues the search by
looking into the directories listed in the value of ``ADA_INCLUDE_PATH``
environment variable. If the source file is found (say in the current
directory), ASIS creates the tree file by calling the compiler:

::

  $ gcc -c -gnatc -gnatt -I./dir -I. -I- foo.ads

If an ASIS ``Context`` is associated with ``-CA`` option, then, when
this ``Context`` is opened, ASIS processes all the tree files located in
the tree search path associated with the ``Context``.

The following further rules define the required combinations of parameters
in the actual ``Parameters`` string:

*
  ``-C1`` and ``-CN`` require ``-FT``

*
  ``-FS`` and ``-FM`` require ``-SA``

.. index:: ASIS_Failed exception
.. index:: Parameter_Error error status

In case an incompatible combination is set, ``ASIS_Failed``
with ``Parameter_Error``
status is raised.

If the actual ``Parameters`` string passed to
``Associate`` contains no parameters, the default parameters
are ``-CA``, ``-FT``, and ``-SA``.

.. index:: Dynamic Context modes

The ``-FS`` and ``-FM`` options define *dynamic Context modes*;
they allow the content of a ``Context`` (that is, the set
of ASIS ``Compilation_Unit``\ s contained in the ``Context``) to be
changed while the ``Context`` is open. See :ref:`Dynamic_Context_Modes` for
more details.

.. index:: Name parameter (to Asis.Ada_Environments.Associate)

For the ``Name`` parameter
of the ``Asis.Ada_Environments.Associate``
query, any string can be passed as an actual parameter.
No verification is performed on the contents, and no semantics are
associated with this parameter.

.. _Implementation_Permissions:

Implementation Permissions
==========================

.. index:: Implementation permissions

This section describes how ASIS-for-GNAT deals with
implementation permissions.

``Asis.Implementation.Permissions`` Queries
-------------------------------------------

.. index:: Asis.Implementation.Permissions queries

The Boolean queries defined in the ``Asis.Implementation.Permissions``
package return the following results:

================================================      ===========
*Query*                                               *Value*
================================================      ===========
``Is_Formal_Parameter_Named_Notation_Supported``      ``True``
``Default_In_Mode_Supported``                         ``True``
``Generic_Actual_Part_Normalized``                    ``False``
``Record_Component_Associations_Normalized``          ``False``
``Is_Prefix_Call_Supported``                          ``True``
``Function_Call_Parameters_Normalized``               ``False``
``Call_Statement_Parameters_Normalized``              ``False``
``Discriminant_Associations_Normalized``              ``False``
``Is_Line_Number_Supported``                          ``True``
``Is_Span_Column_Position_Supported``                 ``True``
``Is_Commentary_Supported``                           ``True``
``Attributes_Are_Supported``                          ``False``
``Implicit_Components_Supported``                     ``False`` (*)
``Object_Declarations_Normalized``                    ``False``
``Predefined_Operations_Supported``                   ``False`` (*)
``Inherited_Declarations_Supported``                  ``True``  (*)
``Inherited_Subprograms_Supported``                   ``True``  (*)
``Generic_Macro_Expansion_Supported``                 ``True``
================================================      ===========

(*) See also :ref:`Processing_Implicit_Elements`.

.. _Processing_Implicit_Elements:

Processing Implicit ``Element``\ s
----------------------------------

.. index:: Implicit Element\ s

ASIS ``Element``\ s represent both explicit and implicit
components of Ada programs.
(An example of an implicit construct is an inherited subprogram
of a derived type.)
Some ASIS queries can return implicit ``Element``\ s (that is,
``Element``\ s representing implicit Ada constructs). Any syntactic or
semantic query should accept an implicit ``Element`` as an ``Element``
parameter, but the ASIS Standard allows an implementation not to support
implicit ``Element``\ s at all, or to support them only partially. If an
implementation does not support the implicit ``Element`` representing
a particular kind of construct, then an ASIS query that is supposed to process
this implicit ``Element`` should return either a ``Nil_Element`` or a
``Nil_Element_List`` depending on whether the query returns a single
``Element`` or an ``Element_List``.

Implicit ``Element``\ s are partially supported by ASIS-for-GNAT.

ASIS-for-GNAT supports implicit ``Element``\ s for the following constructs:

* Derived user-defined subprograms
* Derived enumeration literals
* Derived record components

ASIS-for-GNAT does not
support implicit ``Element``\ s representing implicit declarations of
predefined type operations (such as '`=`', or the '`+`'
operation for numeric types).

Processing Several Contexts at a Time
-------------------------------------

According to the ASIS Standard, the number of ASIS ``Context``\ s that can be
associated and opened at a time, as well as the number of ASIS
``Compilation_Unit``\ s that can be processed at a time, are
implementation specific.
ASIS-for-GNAT does not impose any restriction on the number of
``Context``\ s opened at the same time, or on the number of
``Compilation_Unit``\ s that can be obtained from all the opened
``Context``\ s, as long as the application does not go beyond general
system resource limitations.

.. index:: Implementation limits

However, for a ``Context`` associated with an ``-FS`` or ``-FM``
option, all the trees created 'on the fly' while obtaining
``Compilation_Unit``\ s from this ``Context`` are placed in the current
directory. If the current directory also contains
some tree files belonging to another ``Context``, the latter may become
corrupted. To process more than one ``Context`` safely, an application
should have at most one ``Context`` associated with the ``-FS`` or
``-FM`` option. Moreover, if among ``Context``\ s processed at the same
time there is one that can create trees 'on the fly', then the other
``Context``\ s should not use tree files located in the current directory.

Implementation-Defined Types and Values
---------------------------------------

.. index:: Implementation_Defined_Integer_Type subtype
.. index:: Implementation_Defined_Integer_Constant  named number

All the implementation-defined types, subtypes and values depend on the
subtype ``Implementation_Defined_Integer_Type``
and on the
``Implementation_Defined_Integer_Constant``
defined in package ``Asis``.
ASIS-for-GNAT's declarations for these entities are the same as in the ASIS
Standard:

.. code-block:: ada

  subtype Implementation_Defined_Integer_Type is Integer;
  Implementation_Defined_Integer_Constant : constant := 2**31-1;

All the ASIS (sub)types used as list indexes for ASIS array types have
``Implementation_Defined_Integer_Constant`` as an upper bound.

.. _ASIS_Queries_Having_Specific_Implementation_Permissions_or_Implementation-Specific_Results:

ASIS Queries Having Specific Implementation Permissions or Implementation-Specific Results
==========================================================================================

.. index:: Implementation permissions

This section documents
queries having implementation permissions (given under ``--|IP`` sentinel
in the ASIS definition) and queries whose behavior is otherwise
implementation specific. Such queries are presented below
in their order of appearance in the ASIS Standard.
The clause and subclause numbers shown are those from the ASIS Standard.

.. index:: Debug_Image query

The results returned by the ASIS ``Debug_Image``
queries are discussed in
:ref:`Interpreting_Debug_Images`.

**ASIS 8** ``package Asis.Ada_Environments``

.. index:: Asis.Ada_Environments implementation permissions

ASIS 8.1 ``function Default_Name``

.. index:: Default_Name function (implementation permissions)

*
  Null string is returned.

ASIS 8.2  ``function Default_Parameters``

.. index:: Default_Parameters function (implementation permissions)

*
  Null string is returned;.

ASIS 8.4  ``procedure Open``

.. index:: Open procedure  (implementation permissions)

*
  For a ``Context`` associated with the ``-CA`` option:

  *
    If ``-FS`` is also set, nothing is done.

  *
    If the ``-FT`` or ``-FM`` is set, all the tree files (that is,
    files having :file:`.adt` suffix) in the tree search path
    associated with the ``Context`` are processed.
    ASIS reads in each tree file and checks
    that it was created with
    the ``-gnatc`` option. Tree files that cannot be read in or
    that were not created with the ``-gnatc`` option are ignored.
    For each other tree ASIS collects some 'black-box'
    information about the ``Compilation_Unit``\ s that it represents,
    and performs a consistency check
    for every unit it encounters in the tree (see ASIS-for-GNAT
    User's Guide for a discussion of the consistency
    problem). If any consistency check fails, ``ASIS_Failed``
    is raised and the ``Context`` remains closed.

    .. index:: Consistency checking
    .. index:: ASIS_Failed exception

*
  For a ``Context`` associated with a ``-C1`` or ``-CN`` option,
  ASIS processes all the tree files associated with the ``Context``,
  collecting 'black-box' information and performing consistency
  checks for all the encountered Compilation Units.
  If for any reason a tree file cannot be
  successfully read in for a ``Context`` associated with a ``-C1``
  option, ``ASIS_Failed`` is raised and the ``Context`` remains
  closed.
  If a tree read fails for a ``Context`` associated with a
  ``-CN`` option, an ASIS warning
  is generated and the ``Context`` opening process continues.
  If any consistency check fails, ``ASIS_Failed``
  is raised and the ``Context`` remains closed.

  .. index:: Warning messages
  .. index:: ASIS_Failed exception

**ASIS 9** ``package Asis.Ada_Environments.Containers``

.. index:: Asis.Ada_Environments.Containers implementation permissions

*
  ASIS-for-GNAT supports the trivial ``Container`` model. Every
  ``Context`` contains exactly one ``Container``, whose content and name
  are the same as its enclosing ``Context``

**ASIS 10** ``package Asis.Compilation_Units``

.. index:: Asis.Compilation_Units implementation permissions

ASIS 10.3  ``function Unit_Origin``

.. index:: Unit_Origin function (implementation permissions)

*
  ``A_Predefined_Unit`` origin is returned for those compilation units
  listed in RM95, Annex A(2), and only for these units.

  .. index:: A_Predefined_Unit

*
  ``An_Implementation_Unit`` origin is returned for compilation
  units that are the components of the GNAT Run-Time
  Library, but that are not listed in RM95, Annex A(2).

  .. index:: An_Implementation_Unit

*
  ``An_Application_Unit`` origin is returned for all other
  compilation units.

  .. index:: An_Application_Unit

ASIS 10.6 ``function Library_Unit_Declaration`` and ASIS 10.7
``function Compilation_Unit_Body``

.. index:: Library_Unit_Declaration function (implementation permissions)
.. index:: Compilation_Unit_Body function (implementation permissions)

*
  When processing a ``Context`` associated with an ``-FS`` or
  ``-FM`` option, if ASIS cannot find a needed unit in the tree files
  that have been already processed, it tries to create the needed tree by
  locating the source of the unit and compiling it 'on the fly'. If this
  attempt fails for any reason, ``Nil_Compilation_Unit`` is returned.

ASIS 10.13 ``function Corresponding_Declaration``

.. index:: Corresponding_Declaration function (implementation permissions)

*
  ASIS-for-GNAT does not make use of ASIS ``Compilation_Unit``\ s
  of ``An_Unknown_Unit`` kind.

  .. index:: An_Unknown_Unit

*
  If an argument is of ``A_Public_Declaration_And_Body`` class,
  ``Nil_Compilation_Unit`` is returned.

ASIS 10.14 ``function Corresponding_Body``

.. index:: Corresponding_Body function (implementation permissions)

*
  ASIS-for-GNAT does not make use of ASIS ``Compilation_Unit``\ s
  of ``An_Unknown_Unit`` kind.

ASIS 10.22 ``function Can_Be_Main_Program``

.. index:: Can_Be_Main_Program function (implementation permissions)

*
  For GNAT, any parameterless library procedure and any
  parameterless library function returning a result of an integer type is
  classified by this  query as a (possible) main subprogram for a partition.

*
  If for such a library subprogram both spec and body exist as ASIS
  ``Compilation_Unit``\ s retrievable from a given ASIS ``Context``, both
  are considered as  ``Can_Be_Main_Program``.

ASIS 10.24 ``function Text_Name``

.. index:: Text_Name function (implementation permissions)

*
  This function returns the name of the source file containing the source of
  ``Compilation_Unit``. This name may or may not contain a prefix denoting
  the directory in the underlying file system.  If present, the directory may be
  given in absolute or relative form, depending on the command line options
  that were used for the call to GNAT that created the corresponding tree
  file.

*
  This function does not check the existence of the corresponding source file in
  the underlying file system, it just reflects the situation which was in
  effect when the corresponding tree file was created. Thus, if you delete or
  move the corresponding source file after creating the tree, the full file
  name returned by this function will be incorrect.

*
  Use the query ``Asis.Extensions.Source_File_Status`` to get the information
  about the current status of the source file for a ``Compilation_Unit``.

ASIS 10.25 ``function Text_Form``

.. index:: Text_Form function (implementation permissions)

*
  In the GNAT compilation model all source files are ordinary text files in the
  underlying file system. Therefore this function always returns a
  ``Nil_Asis_String`` to indicate that ``Text_IO.Open`` uses the default
  options for manipulating Ada sources.

ASIS 10.26 ``function Object_Name``

.. index:: Object_Name function (implementation permissions)

*
  Returns a null string. In the GNAT environment, creating an object file has
  no connection with creating trees for ASIS.

ASIS 10.27 ``function Object_Form``

.. index:: Object_Form function (implementation permissions)

*
  Returns a null string.

ASIS 10.29 ``function Has_Attribute``

.. index:: Has_Attribute function (implementation permissions)

*
  Returns ``False``. ASIS-for-GNAT does not provide any additional attributes
  for Compilation Units.

ASIS 10.30 ``function Attribute_Value_Delimiter``

.. index:: Attribute_Value_Delimiter function (implementation permissions)

*
  Returns a wide string of length one containing the ``LF`` wide
  character.

ASIS 10.31 ``function Attribute_Values``

.. index:: Attribute_Values function (implementation permissions)

*
  A null string is returned.

**ASIS 11** ``package Asis.Compilation_Units.Times``

.. index:: Asis.Compilation_Units.Times implementation permissions

ASIS 11.2  ``function Time_Of_Last_Update``

.. index:: Time_Of_Last_Update function (implementation permissions)

*
  This function returns the time stamp (the time of the latest change)
  of the corresponding
  source file. The corresponding source file is the source file whose name is
  returned by ``Asis.Compilation_Units.Text_Name``.

ASIS 11.3  ``function Compilation_CPU_Duration``

.. index:: Compilation_CPU_Duration function (implementation permissions)

*
  This function always returns zero duration, because the CPU compilation
  duration concept does not apply to ASIS-for-GNAT

ASIS 11.4  ``function Attribute_Time``

.. index:: Attribute_Time function (implementation permissions)

*
  This function always returns ``Nil_ASIS_Time`` because
  ASIS-for-GNAT does not provide any ``Compilation_Unit`` attributes

**ASIS 13** ``package Asis.Elements``

.. index:: Asis.Elements implementation permissions

ASIS 13.3  ``function Context_Clause_Elements``

.. index:: Context_Clause_Elements function (implementation permissions)

*
  This function returns exactly those clauses and pragmas that are in the
  source for the unit.

*
  Returns ``Nil_Element_List`` if the argument unit is of
  ``A_Nonexistent_Declaration``, ``A_Nonexistent_Body`` or
  ``An_Unknown_Unit`` kind

*
  Returns ``Nil_Element_List`` for the predefined package ``Standard``.
  For all other predefined Ada compilation units, returns their context clauses
  as they appear in the sources held in the GNAT Run-Time
  Library.

ASIS 13.4  ``function Configuration_Pragmas``

.. index:: Configuration_Pragmas function (implementation permissions)

*
  This function always returns ``Nil_Element_List``, because in the GNAT
  compilation environment "a list of pragmas that apply to all future
  compilation_unit elements compiled into ``The_Context``" essentially
  depends on the GNAT options set when compiling a unit (in particular the
  ``-gnatA`` and ``-gnatec`` options), and this cannot be determined
  from the content of the given ``Context``.

ASIS 13.5  ``function Compilation_Pragmas``

.. index:: Compilation_Pragmas function (implementation permissions)

*
  If the argument unit has been compiled on its own to produce a corresponding
  tree file, then the result contains the configuration pragmas from the
  GNAT configuration file(s) involved in this compilation. Otherwise  (that is,
  if the argument unit has been compiled only as an effect of compiling some
  other unit), the result contains only those pragmas that belong to
  the unit's source file.

*
  A pragma that appears in the unit's context clause is included in the result
  list only if it is a configuration pragma.

*
  Returns ``Nil_Element_List`` for the predefined package ``Standard``.

ASIS 13.31 ``function Is_Equal``

.. index:: Is_Equal function (implementation permissions)

*
  Two elements representing configuration pragmas belonging to
  ``A_Configuration_Compilation`` unit (or components thereof) are considered
  as being equal only if they are created by the same compilation (belong
  to the same tree).

ASIS 13.36 ``function Enclosing_Element``

.. index:: Enclosing_Element function (implementation permissions)

*
  ASIS-for-GNAT does not require the ``Element_Context`` parameter.
  The ``Enclosing_Element`` function with two parameters just calls the
  ``Enclosing_Element`` function with one parameter for its ``Element``
  parameter.

**ASIS 15** ``package Asis.Declarations``

.. index:: Asis.Declarations implementation permissions

ASIS 15.24 ``function Body_Block_Statement``

.. index:: Body_Block_Statement function (implementation permissions)

*
  If the body passed as the actual parameter has no declarative items of its
  own,  ``Asis.Statements.Is_Declare_Block`` returns ``False``.

**ASIS 18** ``package Asis.Statements``

.. index:: Asis.Statements implementation permissions

ASIS 18.14 ``function Is_Declare_Block``

.. index:: Is_Declare_Block function (implementation permissions)

*
  If the argument represents the dummy block statement created by
  ``Asis.Declarations.Body_Block_Statement`` function, the result will be
  ``True`` if and only if the corresponding body has declarative items.

**ASIS 20** ``package Asis.Text``

.. index:: Asis.Text implementation permissions

ASIS 20.1 ``type Line``

.. index:: Line type (implementation permissions)

*
  Lines in ASIS-for-GNAT do not contain any end-of-line characters
  (see RM95, 2.2(2)).

ASIS 20.22 ``function Delimiter_Image``

.. index:: Delimiter_Image function (implementation permissions)

*
  Returns a wide string of length one, containing the ``LF`` wide character.

Processing of Predefined Input-Output Packages
==============================================

.. index:: Processing of Predefined Input-Output Packages

The GNAT compiler transforms the structure of the predefined input-output
packages (`Ada.Text_IO`, ``Ada.Wide_Text_IO`` and
``Ada.Wide_Wide_Text_IO``) to optimize compilations of their clients.
The documentation of ``Ada.Text_IO`` says:

.. code-block:: ada

  --  Note: the generic subpackages of Text_IO (Integer_IO, Float_IO, Fixed_IO,
  --  Modular_IO, Decimal_IO and Enumeration_IO) appear as private children in
  --  GNAT. These children are with'ed automatically if they are referenced, so
  --  this rearrangement is invisible to user programs, but has the advantage
  --  that only the needed parts of Text_IO are processed and loaded.

The same happens for ``Ada.Wide_Text_IO`` and ``Ada.Wide_Wide_Text_IO``.
In this situation ASIS follows not the Ada Standard, but the actual code
contained in the GNAT Run-Time Library. That is, the
``Enclosing_Compilation_Unit`` for an ASIS ``Element`` representing
``Ada.Text_IO.Integer_IO`` will be not the
``Compilation_Unit`` that contains the whole package ``Ada.Text_IO``, but
the ``Compilation_Unit`` representing its private child as it is described
above.

The ``Asis.Extensions`` package contains a query named
``Is_Sub_Package_Implemented_As_Child_Unit`` that allows to detect such
private children of predefined Ada text input-output packages.

Representation clauses and ``-gnatI`` GNAT option
=================================================

.. index:: Representation clauses and -gnatI GNAT option

GNAT ``-gnatI`` allows to ignore all the representation clauses in the
code being compiled. This allows to compile the code if it contains representation
clauses that are illegal in the given compilation environment. ASIS can process
tree files created with ``-gnatI``, and for the ASIS ``Context`` that
is based on such trees, ASIS does not yield ``Elements`` that correspond
to representation clauses.

Note that you will see these representation clauses in the text images of
the enclosing ``Elements``, but nevertheless you will not be able to get
them as subcomponents of such ``Elements``.

.. _Dynamic_Context_Modes:

Dynamic ``Context`` Modes
=========================

.. index:: Dynamic Context modes

If an ASIS ``Context`` is defined with an ``-FS`` or ``-FM``
option, then ASIS may compile sources 'on the fly' to obtain
``Compilation_Unit``\ s.
Thus the content of the ``Context`` will not necessarily remain frozen
when the ``Context`` is open --- when ASIS gets a new
``Compilation_Unit``, it 'adds' it to the ``Context``.
The ``-FS`` and ``-FM`` options
are referred to as *dynamic Context modes*.

The difference between the two modes is as follows:


``-FS``
  ASIS does not take into account any existing tree file when opening a
  ``Context``.

``-FM``
  ASIS first processes the tree files in the tree search
  path.  If a given ``Compilation_Unit`` is present in the existing set of
  tree files, these tree files are used; otherwise ASIS tries to locate the
  source of the unit and to compile it to produce a tree file.

For both ``-FS`` and
``-FM`` ``Context``\ s, once a tree file
is created it is added to the set of tree
files making up the ``Context`` and then it is reused (without recreating
it from sources again) for the queries dealing with ``Compilation_Unit``\ s
represented by this tree.

.. index:: Tree file

An advantage of these dynamic ``Context`` modes is that you do not have to
create the tree files explicitly; to users of an ASIS application based on
such ``Context`` modes the application appears to operate directly from
source files. But there is also a
drawback, a consequence of the fact that the content of a ``Context`` may
change while the ``Context`` is open: some ASIS queries dealing
with ``Compilation_Unit``\ s or returning lists of ``Compilation_Unit``\ s
raise the ``ASIS_Failed``
exception (with ``Use_Error`` status).

.. index:: ASIS_Failed exception
.. index:: Use_Error error status

These queries are as follows:

.. code-block:: ada

  Asis.Compilation_Units:
       Library_Unit_Declarations
       Compilation_Unit_Bodies
       Compilation_Units
       Corresponding_Children

Another limitation of the dynamic ``Context`` mode is that ASIS uses the
standard GNAT naming scheme to compute the name of the source to be compiled
from the name of the corresponding Ada compilation unit. That is, if the name
of the source containing the code of some unit does not follow the GNAT
naming scheme, then ASIS will not locate this source, and it will treat
this unit as ``Nil_Compilation_Unit``.
