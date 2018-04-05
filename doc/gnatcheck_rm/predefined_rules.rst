.. _Predefined_Rules:

****************
Predefined Rules
****************

.. index:: Predefined Rules

The description of the rules currently implemented in *gnatcheck* is
given in this chapter.
The rule identifier is
used as a parameter of *gnatcheck*'s ``+R`` or ``-R``
switches.

Be aware that most of these rules apply to specialized coding
requirements developed by individual users and may well not make sense in
other environments. In particular, there are many rules that conflict
with one another. Proper usage of gnatcheck involves selecting the rules
you wish to apply by looking at your independently developed coding
standards and finding the corresponding gnatcheck rules.

If not otherwise specified, a rule does not do any check for the
results of generic instantiations.

Style-Related Rules
===================

.. index:: Style-related rules

The rules in this section may be used to enforce various feature usages
consistent with good software engineering, for example
as described in Ada 95 Quality and Style.

.. _Tasking:

Tasking
-------

.. index:: Tasking-related rules

The rules in this subsection may be used to enforce various
feature usages related to concurrency.

.. _Multiple_Entries_In_Protected_Definitions:

``Multiple_Entries_In_Protected_Definitions``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Multiple_Entries_In_Protected_Definitions

Flag each protected definition (i.e., each protected object/type declaration)
that declares more than one entry.
Diagnostic messages are generated for all the entry declarations
except the first one. An entry family is counted as one entry. Entries from
the private part of the protected definition are also checked.

This rule has no parameters.

.. _Volatile_Objects_Without_Address_Clauses:

``Volatile_Objects_Without_Address_Clauses``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Volatile_Objects_Without_Address_Clauses

Flag each volatile object that does not have an address clause.

The following check is made: if the pragma ``Volatile`` is applied to a
data object or to its type, then an address clause must
be supplied for this object.

This rule does not check the components of data objects,
array components that are volatile as a result of the pragma
``Volatile_Components``, or objects that are volatile because
they are atomic as a result of pragmas ``Atomic`` or
``Atomic_Components``.

Only variable declarations, and not constant declarations, are checked.

This rule has no parameters.

.. _Object_Orientation:

Object Orientation
------------------

.. index:: Object-Orientation related rules

The rules in this subsection may be used to enforce various
feature usages related to Object-Oriented Programming.

.. _Deep_Inheritance_Hierarchies:

``Deep_Inheritance_Hierarchies``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Deep_Inheritance_Hierarchies

Flags a tagged derived type declaration or an interface type declaration if
its depth (in its inheritance hierarchy) exceeds the value specified by the
*N* rule parameter. Types in generic instantiations which violate this
rule are also flagged; generic formal types are not flagged. This rule also
does not flag private extension declarations. In the case of a private
extension, the corresponding full declaration is checked.

In most cases, the inheritance depth of a tagged type or interface type is
defined as 0 for a type with no parent and no progenitor, and otherwise as 1 +
max of the depths of the immediate parent and immediate progenitors. If the
declaration of a formal derived type has no progenitor, or if the declaration
of a formal interface type has exactly one progenitor, then the inheritance
depth of such a formal derived/interface type is equal to the inheritance
depth of its parent/progenitor type, otherwise the general rule is applied.

If the rule flags a type declaration inside the generic unit, this means that
this type declaration will be flagged in any instantiation of the generic
unit. But if a type is derived from a format type or has a formal progenitor
and it is not flagged at the place where it is defined in a generic unit, it
may or may not be flagged in instantiation, this depends of the inheritance
depth of the actual parameters.

This rule has the following (mandatory) parameter for the ``+R`` option:



*N*
  Integer not less than -1 specifying the maximal allowed depth of any
  inheritance hierarchy. If the rule parameter is set to -1, the rule
  flags all the declarations of tagged and interface types.

.. _Direct_Calls_To_Primitives:

``Direct_Calls_To_Primitives``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Direct_Calls_To_Primitives

Flag any nondispatching call to a dispatching primitive operation, except for:


*
  a call to the corresponding primitive of the parent type.  (This
  occurs in the common idiom where a primitive subprogram for a tagged type
  directly calls the same primitive subprogram of the parent type.)

*
  a call to a primitive of an untagged private type, even though the full type
  may be tagged, when the call is made at a place where the view of the type is
  untagged.


This rule has the following (optional) parameters for the ``+R`` option:



*Except_Constructors*
  Do not flag nondispatching calls to functions if the function has a
  controlling result and no controlling parameters (in a traditional OO sense
  such functions may be considered as constructors).

.. _Too_Many_Parents:

``Too_Many_Parents``
^^^^^^^^^^^^^^^^^^^^

.. index:: Too_Many_Parents

Flag any tagged type declaration, interface type declaration, single task
declaration or single protected declaration that has more than *N*
*parents*, where *N* is a parameter of the rule.
A *parent* here is either a (sub)type denoted by the subtype mark from the
parent_subtype_indication (in case of a derived type declaration), or
any of the progenitors from the interface list (if any).

This rule has the following (mandatory) parameters for the ``+R`` option:



*N*
  Positive integer specifying the maximal allowed number of parents/progenitors.

.. _Visible_Components:

``Visible_Components``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Visible_Components

Flag all the type declarations located in the visible part of a library
package or a library generic package that can declare a visible component.
A visible component can be declared in a *record definition* which appears
on its own or as part of a record extension.  The *record definition* is
flagged even if it contains no components.

*Record definitions* located in private parts of library (generic) packages
or in local (generic) packages are not flagged. *Record definitions* in
private packages, in package bodies, and in the main subprogram body are not
flagged.

This rule has no parameters.

.. _Portability:

Portability
-----------

.. index:: Portability-related rules

The rules in this subsection may be used to enforce various
feature usages that support program portability.

.. _Forbidden_Attributes:

``Forbidden_Attributes``
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Forbidden_Attributes

Flag each use of the specified attributes. The attributes to be detected are
named in the rule's parameters.

This rule has the following parameters:

* For the ``+R`` option



*Attribute_Designator*
    Adds the specified attribute to the set of attributes to be detected and sets
    the detection checks for all the specified attributes ON.
    If *Attribute_Designator*
    does not denote any attribute defined in the Ada standard
    or in the GNAT Reference Manual,
    it is treated as the name of unknown attribute.


``GNAT``
    All the GNAT-specific attributes are detected; this sets
    the detection checks for all the specified attributes ON.


``ALL``
    All attributes are detected; this sets the rule ON.

* For the ``-R`` option


*Attribute_Designator*
    Removes the specified attribute from the set of attributes to be
    detected without affecting detection checks for
    other attributes. If *Attribute_Designator* does not correspond to any
    attribute defined in the Ada standard
    or in the GNAT Reference Manual,
    this option is treated as turning OFF detection of all unknown attributes.


GNAT
    Turn OFF detection of all GNAT-specific attributes


ALL
    Clear the list of the attributes to be detected and
    turn the rule OFF.

Parameters are not case sensitive. If *Attribute_Designator* does not
have the syntax of an Ada identifier and therefore can not be considered as a
(part of an) attribute designator, a diagnostic message is generated and the
corresponding parameter is ignored. (If an attribute allows a static
expression to be a part of the attribute designator, this expression is
ignored by this rule.)

When more than one parameter is given in the same rule option, the parameters
must be separated by commas.

If more than one option for this rule is specified for the gnatcheck call, a
new option overrides the previous one(s).

The ``+R`` option with no parameters turns the rule ON, with the set of
attributes to be detected defined by the previous rule options.
(By default this set is empty, so if the only option specified for the rule is
``+RForbidden_Attributes`` (with
no parameter), then the rule is enabled, but it does not detect anything).
The ``-R`` option with no parameter turns the rule OFF, but it does not
affect the set of attributes to be detected.

The rule allows parametric exemption, the parameters that are allowed in the
definition of exemption sections are *Attribute_Designators*. Each
*Attribute_Designator* used as a rule exemption parameter should denote
a predefined or GNAT-specific attribute.

.. _Forbidden_Pragmas:

``Forbidden_Pragmas``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Forbidden_Pragmas

Flag each use of the specified pragmas.  The pragmas to be detected
are named in the rule's  parameters.

This rule has the following parameters:

* For the ``+R`` option



*Pragma_Name*
    Adds the specified pragma to the set of pragmas to be
    checked and sets the checks for all the specified pragmas
    ON. *Pragma_Name* is treated as a name of a pragma. If it
    does not correspond to any pragma name defined in the Ada
    standard or to the name of a GNAT-specific pragma defined
    in the GNAT Reference Manual,
    it is treated as the name of unknown pragma.


``GNAT``
    All the GNAT-specific pragmas are detected; this sets
    the checks for all the specified pragmas ON.


``ALL``
    All pragmas are detected; this sets the rule ON.

* For the ``-R`` option


*Pragma_Name*
    Removes the specified pragma from the set of pragmas to be
    checked without affecting checks for
    other pragmas. *Pragma_Name* is treated as a name
    of a pragma. If it does not correspond to any pragma
    defined in the Ada standard or to any name defined
    in the GNAT Reference Manual,
    this option is treated as turning OFF detection of all unknown pragmas.


GNAT
    Turn OFF detection of all GNAT-specific pragmas


ALL
    Clear the list of the pragmas to be detected and
    turn the rule OFF.

Parameters are not case sensitive. If *Pragma_Name* does not have
the syntax of an Ada identifier and therefore can not be considered
as a pragma name, a diagnostic message is generated and the corresponding
parameter is ignored.

When more than one parameter is given in the same rule option, the parameters
must be separated by a comma.

If more than one option for this rule is specified for the *gnatcheck*
call, a new option overrides the previous one(s).

The ``+R`` option with no parameters turns the rule ON with the set of
pragmas to be detected defined by the previous rule options.
(By default this set is empty, so if the only option specified for the rule is
``+RForbidden_Pragmas`` (with
no parameter), then the rule is enabled, but it does not detect anything).
The ``-R`` option with no parameter turns the rule OFF, but it does not
affect the set of pragmas to be detected.

Note that in case when the rule is enabled with *ALL* parameter, then
the rule will flag also pragmas ``Annotate`` used to exempt rules, see
:ref:`Rule_exemption`. Even if you exempt this *Forbidden_Pragmas* rule
then the pragma ``Annotate`` that closes the exemption section will be
flagged as non-exempted. To avoid this, turn off the check for pragma
``Annotate`` by using ``-RForbidden_Pragmas:Annotate`` rule option.

The rule allows parametric exemption, the parameters that are allowed in the
definition of exemption sections are pragma names. Each
name used as a rule exemption parameter should denote
a predefined or GNAT-specific pragma.

.. _Implicit_SMALL_For_Fixed_Point_Types:

``Implicit_SMALL_For_Fixed_Point_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Implicit_SMALL_For_Fixed_Point_Types

Flag each fixed point type declaration that lacks an explicit
representation  clause to define its ``'Small`` value.
Since ``'Small`` can be  defined only for ordinary fixed point types,
decimal fixed point type declarations are not checked.

This rule has no parameters.

.. _No_Scalar_Storage_Order_Specified:

``No_Scalar_Storage_Order_Specified``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: No_Scalar_Storage_Order_Specified

Flag each record type declaration, record extension declaration, and
untagged derived record type declaration if a
record_representation_clause that has at least one component clause
applies to it (or an ancestor), but neither the type nor any of its
ancestors has an explicitly specified Scalar_Storage_Order attribute.

This rule has no parameters.

.. _Predefined_Numeric_Types:

``Predefined_Numeric_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Predefined_Numeric_Types

Flag each explicit use of the name of any numeric type or subtype declared
in package ``Standard``.

The rationale for this rule is to detect when the
program may depend on platform-specific characteristics of the implementation
of the predefined numeric types. Note that this rule is overly pessimistic;
for example, a program that uses ``String`` indexing
likely needs a variable of type ``Integer``.
Another example is the flagging of predefined numeric types with explicit
constraints:


.. code-block:: ada

      subtype My_Integer is Integer range Left .. Right;
      Vy_Var : My_Integer;


This rule detects only numeric types and subtypes declared in package
``Standard``. The use of numeric types and subtypes declared in other
predefined packages (such as ``System.Any_Priority`` or
``Ada.Text_IO.Count``) is not flagged

This rule has no parameters.

.. _Separate_Numeric_Error_Handlers:

``Separate_Numeric_Error_Handlers``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Separate_Numeric_Error_Handlers

Flags each exception handler that contains a choice for
the predefined ``Constraint_Error`` exception, but does not contain
the choice for the predefined ``Numeric_Error`` exception, or
that contains the choice for ``Numeric_Error``, but does not contain the
choice for ``Constraint_Error``.

This rule has no parameters.

.. _Program_Structure:

Program Structure
-----------------

.. index:: Program Structure related rules

The rules in this subsection may be used to enforce feature usages
related to program structure.

.. _Deeply_Nested_Generics:

``Deeply_Nested_Generics``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Deeply_Nested_Generics

Flag a generic declaration nested in another generic declaration if
the nesting level of the inner generic exceeds
the value specified by the *N* rule parameter.
The nesting level is the number of generic declarations that enclose the given
(generic) declaration. Formal packages are not flagged by this rule.

This rule has the following (mandatory) parameters for the ``+R`` option:



*N*
  Positive integer specifying the maximum nesting level for a
  generic declaration.

.. _Local_Packages:

``Local_Packages``
^^^^^^^^^^^^^^^^^^

.. index:: Local_Packages

Flag all local packages declared in package and generic package
specs.
Local packages in bodies are not flagged.

This rule has no parameters.

.. _Non_Visible_Exceptions:

``Non_Visible_Exceptions``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Non_Visible_Exceptions rule

Flag constructs leading to the possibility of propagating an exception
out of the scope in which the exception is declared.
Two cases are detected:

*
  An exception declaration in a subprogram body, task body or block
  statement is flagged if the body or statement does not contain a handler for
  that exception or a handler with an ``others`` choice.

*
  A ``raise`` statement in an exception handler of a subprogram body,
  task body or block statement is flagged if it (re)raises a locally
  declared exception.  This may occur under the following circumstances:

  *
    it explicitly raises a locally declared exception, or
  *
    it does not specify an exception name (i.e., it is simply ``raise;``)
    and the enclosing handler contains a locally declared exception in its
    exception choices.

Renamings of local exceptions are not flagged.

This rule has no parameters.

.. _Raising_External_Exceptions:

``Raising_External_Exceptions``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Raising_External_Exceptions

Flag any ``raise`` statement, in a program unit declared in a library
package or in a generic library package, for an exception that is
neither a predefined exception nor an exception that is also declared (or
renamed) in the visible part of the package.

This rule has no parameters.

.. _Programming_Practice:

Programming Practice
--------------------

.. index:: Programming Practice related rules

The rules in this subsection may be used to enforce feature usages that
relate to program maintainability.

.. _Anonymous_Arrays:

``Anonymous_Arrays``
^^^^^^^^^^^^^^^^^^^^

.. index:: Anonymous_Arrays

Flag all anonymous array type definitions (by Ada semantics these can only
occur in object declarations).

This rule has no parameters.

.. _Binary_Case_Statements:

``Binary_Case_Statements``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Binary_Case_Statements

Flag a case statement if this statement has only two alternatives, one
containing exactly one choice, the other containing exactly one choice
or the ``OTHERS`` choice.

This rule has no parameters.

.. _Default_Values_For_Record_Components:

``Default_Values_For_Record_Components``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Default_Values_For_Record_Components

Flag a record component declaration if it contains a default expression.
Do not flag record component declarations in protected definitions.
Do not flag discriminant specifications.

This rule has no parameters.

.. _Deriving_From_Predefined_Type:

``Deriving_From_Predefined_Type``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Deriving_From_Predefined_Type

Flag derived type declaration if the ultimate ancestor type is a
predefined Ada type. Do not flag record extensions and private
extensions. The rule is checked inside expanded generics.

This rule has no parameters.

.. _Enumeration_Ranges_In_CASE_Statements:

``Enumeration_Ranges_In_CASE_Statements``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Enumeration_Ranges_In_CASE_Statements

Flag each use of a range of enumeration literals as a choice in a
``case`` statement.
All forms for specifying a range (explicit ranges
such as ``A .. B``, subtype marks and ``'Range`` attributes) are flagged.
An enumeration range is
flagged even if contains exactly one enumeration value or no values at all. A
type derived from an enumeration type is considered as an enumeration type.

This rule helps prevent maintenance problems arising from adding an
enumeration value to a type and having it implicitly handled by an existing
``case`` statement with an enumeration range that includes the new literal.

This rule has no parameters.

.. _Enumeration_Representation_Clauses:

``Enumeration_Representation_Clauses``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Enumeration_Representation_Clauses

Flag enumeration representation clauses.

This rule has no parameters.

.. _Exceptions_As_Control_Flow:

``Exceptions_As_Control_Flow``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Exceptions_As_Control_Flow

Flag each place where an exception is explicitly raised and handled in the
same subprogram body. A ``raise`` statement in an exception handler,
package body, task body or entry body is not flagged.

The rule has no parameters.

.. _Exits_From_Conditional_Loops:

``Exits_From_Conditional_Loops``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Exits_From_Conditional_Loops

Flag any exit statement if it transfers the control out of a ``for`` loop
or a ``while`` loop. This includes cases when the ``exit`` statement
applies to a ``FOR`` or ``while`` loop, and cases when it is enclosed
in some ``for`` or ``while`` loop, but transfers the control from some
outer (unconditional) ``loop`` statement.

The rule has no parameters.

.. _EXIT_Statements_With_No_Loop_Name:

``EXIT_Statements_With_No_Loop_Name``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: EXIT_Statements_With_No_Loop_Name

Flag each ``exit`` statement that does not specify the name of the loop
being exited.

The rule has no parameters.

.. _Global_Variables:

``Global_Variables``
^^^^^^^^^^^^^^^^^^^^

.. index:: Global_Variables

Flag any variable declaration that appears immediately within the
specification of a library package or library generic package. Variable
declarations in nested packages and inside package instantiations are
not flagged.

This rule has the following (optional) parameters for the ``+R`` option:



*Only_Public*
  Do not flag variable declarations in private library (generic) packages and
  in package privat parts.

.. _GOTO_Statements:

``GOTO_Statements``
^^^^^^^^^^^^^^^^^^^

.. index:: GOTO_Statements

Flag each occurrence of a ``goto`` statement.

This rule has no parameters.

.. _Improper_Returns:

``Improper_Returns``
^^^^^^^^^^^^^^^^^^^^

.. index:: Improper_Returns

Flag each explicit ``return`` statement in procedures, and
multiple ``return`` statements in functions.
Diagnostic messages are generated for all ``return`` statements
in a procedure (thus each procedure must be written so that it
returns implicitly at the end of its statement part),
and for all ``return`` statements in a function after the first one.
This rule supports the stylistic convention that each subprogram
should have no more than one point of normal return.

This rule has no parameters.

.. _Maximum_Parameters:

``Maximum_Parameters``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Maximum_Parameters

Flag any subprogram declaration, subprogram body declaration, expression
function declaration, null procedure declaration, subprogram
body stub or generic subprogram declaration if the corresponding
subprogram has more than *N* formal parameters, where *N* is a
parameter of the rule.

A subprogram body, an expression function, a null procedure or
a subprogram body stub is flagged only if there is
no separate declaration for this subprogram. Subprogram renaming
declarations and subprogram instantiations, as well as declarations
inside expanded generic instantiations are never flagged.

This rule has the following (mandatory) parameters for the ``+R`` option:



*N*
  Positive integer specifying the maximum allowed total number of
  subprogram formal parameters.

.. _Nested_Subprograms:

``Nested_Subprograms``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Nested_Subprograms

Flag any subprogram declaration, subprogram body declaration, subprogram
instantiation, expression function declaration or subprogram body stub
that is not a completion of another subprogram declaration and that is
declared within subprogram body (including bodies of generic
subprograms), task body or entry body directly or indirectly (that is -
inside a local nested package). Protected subprograms are not flagged.
Null procedure declarations are not flagged. Procedure declarations
completed by null procedure declarations are not flagged.

This rule has no parameters.

.. _Non_Short_Circuit_Operators:

``Non_Short_Circuit_Operators``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Non_Short_Circuit_Operators

Flag all calls to predefined ``and`` and ``or`` operators for
any boolean type. Calls to
user-defined ``and`` and ``or`` and to operators defined by renaming
declarations are not flagged. Calls to predefined ``and`` and ``or``
operators for modular types or boolean array types are not flagged.

This rule has no parameters.

.. _Null_Paths:

``Null_Paths``
^^^^^^^^^^^^^^

.. index:: Null_Paths

Flag a statement sequence that is a component of an IF, CASE or LOOP
statement if this sequences consists of NULL statements only.

This rule has no parameters.

.. _Objects_Of_Anonymous_Types:

``Objects_Of_Anonymous_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Objects_Of_Anonymous_Types

Flag any object declaration located immediately within a package
declaration or a package body (including generic packages) if it uses
anonymous access or array type definition. Record component definitions
and parameter specifications are not flagged. Formal object declarations
defined with anonymous access definitions are flagged.

This rule has no parameters.

.. _OTHERS_In_Aggregates:

``OTHERS_In_Aggregates``
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: OTHERS_In_Aggregates

Flag each use of an ``others`` choice in extension aggregates.
In record and array aggregates, an ``others`` choice is flagged unless
it is used to refer to all components, or to all but one component.

If, in case of a named array aggregate, there are two associations, one
with an ``others`` choice and another with a discrete range, the
``others`` choice is flagged even if the discrete range specifies
exactly one component; for example, ``(1..1 => 0, others => 1)``.

This rule has no parameters.

.. _OTHERS_In_CASE_Statements:

``OTHERS_In_CASE_Statements``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: OTHERS_In_CASE_Statements

Flag any use of an ``others`` choice in a ``case`` statement.

This rule has no parameters.

.. _OTHERS_In_Exception_Handlers:

``OTHERS_In_Exception_Handlers``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: OTHERS_In_Exception_Handlers

Flag any use of an ``others`` choice in an exception handler.

This rule has no parameters.

.. _Overly_Nested_Control_Structures:

``Overly_Nested_Control_Structures``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Overly_Nested_Control_Structures

Flag each control structure whose nesting level exceeds the value provided
in the rule parameter.

The control structures checked are the following:

* ``if`` statement
* ``case`` statement
* ``loop`` statement
* selective accept statement
* timed entry call statement
* conditional entry call statement
* asynchronous select statement

The rule has the following parameter for the ``+R`` option:



*N*
  Positive integer specifying the maximal control structure nesting
  level that is not flagged

If the parameter for the ``+R`` option is not specified or
if it is not a positive integer, ``+R`` option is ignored.

If more than one  option is specified for the gnatcheck call,
the later option and new parameter override the previous one(s).

.. _POS_On_Enumeration_Types:

``POS_On_Enumeration_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: POS_On_Enumeration_Types

Flag ``'Pos`` attribute in case if the attribute prefix has an enumeration
type (including types derived from enumeration types).

This rule has no parameters.

.. _Positional_Actuals_For_Defaulted_Generic_Parameters:

``Positional_Actuals_For_Defaulted_Generic_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Actuals_For_Defaulted_Generic_Parameters

Flag each generic actual parameter corresponding to a generic formal
parameter with a default initialization, if positional notation is used.

This rule has no parameters.

.. _Positional_Actuals_For_Defaulted_Parameters:

``Positional_Actuals_For_Defaulted_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Actuals_For_Defaulted_Parameters

Flag each actual parameter to a subprogram or entry call where the
corresponding formal parameter has a default expression, if positional
notation is used.

This rule has no parameters.

.. _Positional_Components:

``Positional_Components``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Components

Flag each array, record and extension aggregate that includes positional
notation.

This rule has no parameters.

.. _Positional_Generic_Parameters:

``Positional_Generic_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Generic_Parameters

Flag each positional actual generic parameter except for the case when
the generic unit being instantiated has exactly one generic formal
parameter.

This rule has no parameters.

.. _Positional_Parameters:

``Positional_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Parameters

Flag each positional parameter notation in a subprogram or entry call,
except for the following:

*
  Parameters of calls to attribute subprograms are not flagged;
*
  Parameters of prefix or infix calls to operator functions are not flagged;
*
  If the called subprogram or entry has only one formal parameter,
  the parameter of the call is not flagged;
*
  If a subprogram call uses the *Object.Operation* notation, then

  *
    the first parameter (that is, *Object*) is not flagged;
  *
    if the called subprogram has only two parameters, the second parameter
    of the call is not flagged;

This rule has the following (optional) parameters for the ``+R`` option:



*All*
  if this parameter is specified, all the positional parameter
  associations that can be replaced with named associations
  according to language rules are flagged

This rule has no parameters.

.. _Recursive_Subprograms:

``Recursive_Subprograms``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Recursive_Subprograms

Flags specs (and bodies that act as specs) of recursive subprograms. A
subprogram is considered as recursive in a given context if there exists
a chain of direct calls starting from the body of, and ending at
this subprogram within this context. A context is provided by the set
of Ada sources specified as arguments of a given gnatcheck call.
Neither dispatching calls nor calls through access-to-subprograms
are considered as direct calls by this rule.

Generic subprograms and subprograms detected in generic units are not
flagged. Recursive subprograms in expanded generic instantiations
are flagged.

This rule has no parameters.

.. _Unchecked_Address_Conversions:

``Unchecked_Address_Conversions``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unchecked_Address_Conversions

Flag instantiations of ``Ada.Unchecked_Conversion`` if the actual for the
formal type Source is the ``System.Address`` type (or a type derived from
it), and the actual for the formal type ``Target`` is an access type
(including types derived from access types). This include cases when the
actual for ``Source`` is a private type and its full declaration is a type
derived from ``System.Address``, and cases when the actual for ``Target`` is
a private type and its full declaration is an access type. The rule is
checked inside expanded generics.

This rule has no parameters.

.. _Unchecked_Conversions_As_Actuals:

``Unchecked_Conversions_As_Actuals``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unchecked_Conversions_As_Actuals

Flag call to instantiation of ``Ada.Unchecked_Conversion`` if it is an actual in
procedure or entry call or if it is a default value in a subprogram or
entry parameter specification.

This rule has no parameters.

.. _Unconditional_Exits:

``Unconditional_Exits``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unconditional_Exits

Flag unconditional ``exit`` statements.

This rule has no parameters.

.. _Uninitialized_Global_Variables:

``Uninitialized_Global_Variables``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Uninitialized_Global_Variables

Flag an object declaration located immediately within a package
declaration, a generic package declaration or a package body, if it does
not have an explicit initialization. Do not flag deferred constant
declarations and declarations of objects of limited types.

This rule has no parameters.

.. _Unnamed_Blocks_And_Loops:

``Unnamed_Blocks_And_Loops``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unnamed_Blocks_And_Loops

Flag each unnamed block statement and loop statement.

The rule has no parameters.

.. _USE_PACKAGE_Clauses:

``USE_PACKAGE_Clauses``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: USE_PACKAGE_Clauses

Flag all ``use`` clauses for packages; ``use type`` clauses are
not flagged.

This rule has no parameters.

.. _Readability:

Readability
-----------

.. index:: Readability-related rules

The rules described in this subsection may be used to enforce feature usages
that contribute towards readability.

.. _Identifier_Casing:

``Identifier_Casing``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Identifier_Casing

Flag each defining identifier that does not have a casing corresponding to the
kind of entity being declared. All defining names are checked. For the
defining names from the following kinds of declarations a special casing scheme
can be defined:

*
  type and subtype declarations;

*
  enumeration literal specifications (not including character literals)
  and function renaming declarations if the renaming entity is an
  enumeration literal;

*
  constant and number declarations (including object renaming
  declarations if the renamed object is a constant);

*
  exception declarations and exception renaming declarations.

The rule may have the following parameters for ``+R``:



*
  Type=\ *casing_scheme*

  Specifies casing for names from type and subtype declarations.


*
  Enum=\ *casing_scheme*

  Specifies the casing of defining enumeration literals and for the
  defining names in a function renaming declarations if the renamed
  entity is an enumeration literal.


*
  Constant=\ *casing_scheme*

  Specifies the casing for defining names from constants and named number
  declarations, including the object renaming declaration if the
  renamed object is a constant


*
  Exception=\ *casing_scheme*

  Specifies the casing for names from exception declarations and exception
  renaming declarations.


*
  Others=\ *casing_scheme*

  Specifies the casing for all defining names for which no special casing
  scheme is specified. If this parameter is not set, the casing for the
  entities that do not correspond to the specified parameters is not checked.


*
  Exclude=\ *dictionary_file*

  Specifies casing exceptions.

Where:


::

     casing_scheme ::= upper|lower|mixed


*upper* means that the defining identifier should be upper-case.
*lower* means that the defining identifier should be lower-case
*mixed* means that the first defining identifier letter and the first
letter after each underscore should be upper-case, and all the other
letters should be lower-case

If a defining identifier is from a declaration for which a specific casing
scheme can be set, but the corresponding parameter is not specified for the
rule, then the casing scheme defined by ``Others`` parameter is used to
check this identifier. If ``Others`` parameter also is not set, the
identifier is not checked.

*dictionary_file* is the name of the text file that contains casing
exceptions. The way how this rule is using the casing exception dictionary
file is consistent with using the casing exception dictionary in the
GNAT pretty-printer *gnatpp*, see
GNAT User's Guide.

There are two kinds of exceptions:



*identifier*
  If a dictionary file contains an identifier, then each occurrence of that
  (defining) identifier in the checked source should use the casing specified
  included in *dictionary_file*


*wildcard*
  A wildcard has the following syntax


::

      wildcard ::= *simple_identifier* |
                         *simple_identifier |
                         simple_identifier*
      simple_identifier ::= letter{letter_or_digit}


``simple_identifier`` specifies the casing of subwords
(the term 'subword'
is used below to denote the part of a name which is delimited by '_' or by
the beginning or end of the word and which does not contain any '_' inside).
A wildcard of the form ``simple_identifier*`` defines the casing of the
first subword of a defining name to check, the wildcard of the form
``*simple_identifier`` specifies the casing of the last subword, and
the wildcard of the form ``*simple_identifier*`` specifies the casing of
any subword.

If for a defining identifier some of its subwords can be mapped onto
wildcards, but some other cannot, the casing of the identifier subwords
that are not mapped onto wildcards from casing exception dictionary
is checked against the casing scheme defined for the corresponding
entity.

If some identifier is included in the exception dictionary both as a whole
identifier and can be mapped onto some wildcard from the
dictionary, then it is the identifier and not the wildcard that is used to check
the identifier casing.

If more than one dictionary file is specified, or a dictionary file contains
more than one exception variant for the same identifier, the new casing
exception overrides the previous one.

Casing check against dictionary file(s) has a higher priority than checks
against the casing scheme specified for a given entity/declaration kind.

``+R`` option should contain at least one parameter.

There is no parameter for ``-R`` option, it just turns the rule off.

The rule allows parametric exemption, the parameters that are allowed in
the definition of exemption sections are:



*Type*
  Exempts check for type and subtype name casing


*Enum*
  Exempts check for enumeration literal name casing


*Constant*
  Exempts check for constant name casing


*Exception*
  Exempts check for exception name casing


*Others*
  Exempts check for defining names for which no special casing scheme is specified.


*Exclude*
  Exempts check for defining names for which casing schemes are specified in exception
  dictionaries


.. _Identifier_Prefixes:

``Identifier_Prefixes``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Identifier_Prefixes

Flag each defining identifier that does not have a prefix corresponding
to the kind of declaration it is defined by. The defining names in the
following kinds of declarations are checked:

*
  type and subtype declarations (task, protected and access types are treated
  separately);

*
  enumeration literal specifications (not including character literals)
  and function renaming declarations if the renaming entity is an
  enumeration literal;

*
  exception declarations and exception renaming declarations;

*
  constant and number declarations (including object renaming
  declarations if the renamed object is a constant).

Defining names declared by single task declarations or single protected
declarations are not checked by this rule.

The defining name from the full type declaration corresponding to a
private type declaration or a private extension declaration is never
flagged. A defining name from an incomplete type declaration is never
flagged.

The defining name from a subprogram renaming-as-body declaration is
never flagged.

For a deferred constant, the defining name in the corresponding full
constant declaration is never flagged.

The defining name from a body that is a completion of a program unit
declaration or a proper body of a subunit is never flagged.

The defining name from a body stub that is a completion of a program
unit declaration is never flagged.

Note that the rule checks only defining names. Usage name occurrence are
not checked and are never flagged.

The rule may have the following parameters:

*
  For the ``+R`` option:


*
  Type=\ *string*

    Specifies the prefix for a type or subtype name.


*
  Concurrent=\ *string*

    Specifies the prefix for a task and protected type/subtype name. If this
    parameter is set, it overrides for task and protected types the prefix set by
    the Type parameter.


*
  Access=\ *string*

    Specifies the prefix for an access type/subtype name. If this parameter is
    set, it overrides for access types the prefix set by the ``Type``
    parameter.


*
  Class_Access=\ *string*

    Specifies the prefix for the name of an access type/subtype that points to some
    class-wide type. If this parameter is set, it overrides for such access types
    and subtypes the prefix set by the ``Type`` or ``Access`` parameter.


*
  Subprogram_Access=\ *string*

    Specifies the prefix for the name of an access type/subtype that points to a
    subprogram. If this parameter is set, it overrides for such access
    types/subtypes the prefix set by the ``Type`` or ``Access`` parameter.


*
  Derived=\ *string1:string2*

    Specifies the prefix for a type that is directly derived from a given type or
    from a subtype thereof. *string1* should be a full expanded Ada name of the
    ancestor type (starting from the full expanded compilation unit
    name), *string2* defines the prefix to check. If this
    parameter is set, it overrides for types that are directly derived from the
    given type the prefix set by the ``Type`` parameter.


*
  Constant=\ *string*

    Specifies the prefix for defining names from constants and named number
    declarations, including the object renaming declaration if the
    renamed object is a constant


*
  Enum=\ *string*

    Specifies the prefix for defining enumeration literals and for the
    defining names in a function renaming declarations if the renamed
    entity is an enumeration literal.


*
  Exception=\ *string*

    Specifies the prefix for defining names from exception declarations
    and exception renaming declarations.


*Exclusive*
    Check that only those kinds of names for which specific prefix is defined have
    that prefix (e.g., only type/subtype names have prefix *T_*, but
    not variable or package names), and flag all defining names that have any
    of the specified prefixes but do not belong to the kind of entities this
    prefix is defined for. By default the exclusive check mode is ON.

  For the ``-R`` option:


*All_Prefixes*
    Removes all the prefixes specified for the identifier prefix
    checks, whether by default or as specified by other rule
    parameters and disables the rule.


*Type*
    Removes the prefix specified for type/subtype names. This does not remove
    prefixes specified for specific type kinds and does not disable checks for
    these specific kinds.


*Concurrent*
    Removes the prefix specified for task and protected types.


*Access*
    Removes the prefix specified for access types. This does not remove prefixes
    specified for specific access types (access to subprograms and class-wide
    access)


*Class_Access*
    Removes the prefix specified for access types pointing to class-wide types.


*Subprogram_Access*
    Removes the prefix specified for access types pointing to subprograms.


*Derived*
    Removes prefixes specified for derived types that are directly derived from
    specific types.


*Constant*
    Removes the prefix specified for constant and number names and turns off the
    check for these names.


*Exception*
    Removes the prefix specified for exception names and turns off the
    check for exception names.


*Enum*
    Removes the prefix specified for enumeration literal names and
    turns off the check for them.


*Exclusive*
    Turns of the check that only names of specific kinds of entities have prefixes
    specified for these kinds.

If more than one parameter is used, parameters must be separated by
commas.

If more than one option is specified for the gnatcheck invocation, a new
option overrides the previous one(s).

The ``+RIdentifier_Prefixes`` option (with no parameter) enables checks
for all the name prefixes specified by previous options used for this
rule. If no prefix is specified, the rule is not enabled.

The ``-RIdentifier_Prefixes`` option (with no parameter) disables all the
checks but keeps all the prefixes specified by previous options used for
this rule.

There is no default prefix setting for this rule. All checks for
name prefixes are case-sensitive

If any error is detected in a rule parameter, that parameter is ignored.
In such a case the options that are set for the rule are not specified.

The rule allows parametric exemption, the parameters that are allowed in
the definition of exemption sections are:



*Type*
  Exempts check for type and subtype name prefixes


*Concurrent*
  Exempts check for task and protected type/subtype name prefixes


*Access*
  Exempts check for access type/subtype name prefixes


*Class_Access*
  Exempts check for names of access types/subtypes that point to
  some class-wide types


*Subprogram_Access*
  Exempts check for names of access types/subtypes that point to
  subprograms


*Derived*
  Exempts check for derived type name prefixes


*Constant*
  Exempts check for constant and number name prefixes


*Exception*
  Exempts check for exception name prefixes


*Enum*
  Exempts check for enumeration literal name prefixes


*Exclusive*
  Exempts check that only names of specific kinds of entities have prefixes
  specified for these kinds


.. _Identifier_Suffixes:

``Identifier_Suffixes``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Identifier_Suffixes

Flag the declaration of each identifier that does not have a suffix
corresponding to the kind of entity being declared.
The following declarations are checked:

*
  type declarations

*
  subtype declarations

*
  object declarations (variable and constant declarations, but not number,
  declarations, record component declarations, parameter specifications,
  extended return object declarations, formal object declarations)

*
  package renaming declarations (but not generic package renaming
  declarations)

The default checks (enforced by the *Default* rule parameter) are:

*
  type-defining names end with ``_T``, unless the type is an access type,
  in which case the suffix must be ``_A``
*
  constant names end with ``_C``
*
  names defining package renamings end with ``_R``
*
  the check for access type objects is not enabled

Defining identifiers from incomplete type declarations are never flagged.

For a private type declaration (including private extensions), the defining
identifier from the private type declaration is checked against the type
suffix (even if the corresponding full declaration is an access type
declaration), and the defining identifier from the corresponding full type
declaration is not checked.

For a deferred constant, the defining name in the corresponding full constant
declaration is not checked.

Defining names of formal types are not checked.

Check for the suffix of access type data objects is applied to the
following kinds of declarations:

*
  variable and constant declaration

*
  record component declaration

*
  return object declaration

*
  parameter specification

*
  extended return object declaration

*
  formal object declaration

If both checks for constant suffixes and for access object suffixes are
enabled, and if different suffixes are defined for them, then for constants
of access type the check for access object suffixes suffixes is applied.

The rule may have the following parameters:

*
  For the ``+R`` option (unless the parameter
  is ``Default``, then only the explicitly specified
  suffix is checked, and no defaults are used):


*Default*
    Sets the default listed above for all the names to be checked.


*
  Type_Suffix=\ *string*

    Specifies the suffix for a type name.


*
  Access_Suffix=\ *string*

    Specifies the suffix for an access type name. If
    this parameter is set, it overrides for access
    types the suffix set by the ``Type_Suffix`` parameter.
    For access types, *string* may have the following format:
    *suffix1(suffix2)*. That means that an access type name
    should have the *suffix1* suffix except for the case when
    the designated type is also an access type, in this case the
    type name should have the *suffix1 & suffix2* suffix.


*
  Class_Access_Suffix=\ *string*

    Specifies the suffix for the name of an access type that points to some
    class-wide type.
    If this parameter is set, it overrides for such access
    types the suffix set by the ``Type_Suffix`` or ``Access_Suffix``
    parameter.


*
  Class_Subtype_Suffix=\ *string*

    Specifies the suffix for the name of a subtype that denotes a class-wide type.


*
  Constant_Suffix=\ *string*

    Specifies the suffix for a constant name.


*
  Renaming_Suffix=\ *string*

    Specifies the suffix for a package renaming name.

*
  Access_Obj_Suffix=\ *string*

    Specifies the suffix for objects that have an access type
    (including types derived from access types).


*
  For the ``-R`` option:


*All_Suffixes*
    Remove all the suffixes specified for the
    identifier suffix checks, whether by default or
    as specified by other rule parameters. All the
    checks for this rule are disabled as a result.


*Type_Suffix*
    Removes the suffix specified for types. This
    disables checks for types but does not disable
    any other checks for this rule (including the
    check for access type names if ``Access_Suffix`` is
    set).


*Access_Suffix*
    Removes the suffix specified for access types.
    This disables checks for access type names but
    does not disable any other checks for this rule.
    If ``Type_Suffix`` is set, access type names are
    checked as ordinary type names.


*Class_Access_Suffix*
    Removes the suffix specified for access types pointing to class-wide
    type. This disables specific checks for names of access types pointing to
    class-wide types but does not disable any other checks for this rule.
    If ``Type_Suffix`` is set, access type names are
    checked as ordinary type names. If ``Access_Suffix`` is set, these
    access types are checked as any other access type name.


*Class_Subtype_Suffix*
    Removes the suffix specified for subtype names.
    This disables checks for subtype names but
    does not disable any other checks for this rule.


*Constant_Suffix*
    Removes the suffix specified for constants. This
    disables checks for constant names but does not
    disable any other checks for this rule.


*Renaming_Suffix*
    Removes the suffix specified for package
    renamings. This disables checks for package
    renamings but does not disable any other checks
    for this rule.

*Access_Obj_Suffix*
    Removes the suffix specified for objects of access types,
    this disables checks for such objects. It does not disable
    any other checks for this rule


If more than one parameter is used, parameters must be separated by commas.

If more than one  option is specified for the *gnatcheck* invocation,
a new option overrides the previous one(s).

The ``+RIdentifier_Suffixes`` option (with no parameter) enables
checks for all the
name suffixes specified by previous options used for this rule.

The ``-RIdentifier_Suffixes`` option (with no parameter) disables
all the checks but keeps
all the suffixes specified by previous options used for this rule.

The *string* value must be a valid suffix for an Ada identifier (after
trimming all the leading and trailing space characters, if any).
Parameters are not case sensitive, except the *string* part.

If any error is detected in a rule parameter, the parameter is ignored.
In such a case the options that are set for the rule are not
specified.

The rule allows parametric exemption, the parameters that are allowed in
the definition of exemption sections are:



*Type*
  Exempts check for type name suffixes


*Access*
  Exempts check for access type name suffixes


*Access_Obj*
  Exempts check for access object name suffixes


*Class_Access*
  Exempts check for names of access types that point to
  some class-wide types


*Class_Subtype*
  Exempts check for names of subtypes that denote class-wide types


*Constant*
  Exempts check for constant name suffixes


*Renaming*
  Exempts check for package renaming name suffixes


.. _Misnamed_Controlling_Parameters:

``Misnamed_Controlling_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Misnamed_Controlling_Parameters

Flag a declaration of a dispatching operation, if the first parameter is
not a controlling one and its name is not ``This`` (the check for
parameter name is not case-sensitive). Declarations of dispatching functions
with a controlling result and no controlling parameter are never flagged.

A subprogram body declaration, subprogram renaming declaration, or subprogram
body stub is flagged only if it is not a completion of a prior subprogram
declaration.

This rule has no parameters.

.. _Name_Clashes:

``Name_Clashes``
^^^^^^^^^^^^^^^^

.. index:: Name_Clashes

Check that certain names are not used as defining identifiers. The names that
should not be used as identifiers must be listed in a dictionary file that is
a rule parameter. A defining identifier is flagged if it is included in a
dictionary file specified as a rule parameter, the check is not case-sensitive.
More than one dictionary file can be specified as the rule parameter, in this
case the rule checks defining identifiers against the union of all the
identifiers from all the dictionary files provided as the rule parameters.

This rule has the following (mandatory) parameters for the ``+R`` option:



*dictionary_file*
  The name of a dictionary file.

This rule is enabled by default, but without setting any corresponding
dictionary file(s); thus the default effect is to do no checks.

A dictionary file is a plain text file. The maximum line length for this file
is 1024 characters.  If the line is longer than this limit, extra characters
are ignored.

Each line can be either an empty line, a comment line, or a line containing
a list of identifiers separated by space or HT characters.
A comment is an Ada-style comment (from ``--`` to end-of-line).
Identifiers must follow the Ada syntax for identifiers.
A line containing one or more identifiers may end with a comment.

.. _Uncommented_BEGIN_In_Package_Bodies:

``Uncommented_BEGIN_In_Package_Bodies``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Uncommented_BEGIN_In_Package_Bodies

Flags each package body with declarations and a statement part that does not
include a trailing comment on the line containing the ``begin`` keyword;
this trailing comment needs to specify the package name and nothing else.
The ``begin`` is not flagged if the package body does not
contain any declarations.

If the ``begin`` keyword is placed on the
same line as the last declaration or the first statement, it is flagged
independently of whether the line contains a trailing comment. The
diagnostic message is attached to the line containing the first statement.

This rule has no parameters.

.. _Source_Code_Presentation:

Source Code Presentation
------------------------

.. index:: Source code presentation related rules

This subsection is a placeholder; there are currently no rules
in this category.

Feature Usage Rules
===================

.. index:: Feature usage related rules

The rules in this section can be used to enforce specific
usage patterns for a variety of language features.

.. _Abstract_Type_Declarations:

``Abstract_Type_Declarations``
------------------------------

.. index:: Abstract_Type_Declarations

Flag all declarations of abstract types. For an abstract private
type, both the private and full type declarations are flagged.

This rule has no parameters.

.. _Anonymous_Subtypes:

``Anonymous_Subtypes``
----------------------

.. index:: Anonymous_Subtypes

Flag all uses of anonymous subtypes (except cases when subtype indication
is a part of a record component definition, and this subtype indication
depends on a discriminant). A use of an anonymous subtype is
any instance of a subtype indication with a constraint, other than one
that occurs immediately within a subtype declaration. Any use of a range
other than as a constraint used immediately within a subtype declaration
is considered as an anonymous subtype.

The rule does not flag ranges in the component clauses from a record
representation clause, because the language rules do not allow to use
subtype names there.

An effect of this rule is that ``for`` loops such as the following are
flagged (since ``1..N`` is formally a 'range'):


.. code-block:: ada

  for I in 1 .. N loop
     ...
  end loop;


Declaring an explicit subtype solves the problem:


.. code-block:: ada

  subtype S is Integer range 1..N;
  ...
  for I in S loop
     ...
  end loop;


This rule has no parameters.

.. _Blocks:

``Blocks``
----------

.. index:: Blocks

Flag each block statement.

This rule has no parameters.

.. _Complex_Inlined_Subprograms:

``Complex_Inlined_Subprograms``
-------------------------------

.. index:: Complex_Inlined_Subprograms

Flag a subprogram (or generic subprogram, or instantiation of a subprogram) if
pragma Inline is applied to it and at least one of the following
conditions is met:

*
  it contains at least one complex declaration such as a subprogram body,
  package, task, protected declaration, or a generic instantiation
  (except instantiation of ``Ada.Unchecked_Conversion``);

*
  it contains at least one complex statement such as a loop, a case
  or an if statement;

*
  the number of statements exceeds
  a value specified by the *N* rule parameter;

Subprogram renamings are also considered.

This rule has the following (mandatory) parameter for the ``+R`` option:



*N*
  Positive integer specifying the maximum allowed total number of statements
  in the subprogram body.

.. _Conditional_Expressions:

``Conditional_Expressions``
---------------------------

.. index:: Conditional_Expressions

Flag use of conditional expression.

This rule has the following (optional) parameters for the ``+R`` option:



*Except_Assertions*
  Do not flag a conditional expression if it is a subcomponent
  of the following constructs:



*argument of the following pragmas*


*Language-defined*

*
  ``Assert``


*GNAT-specific*

*
  ``Assert_And_Cut``

*
  ``Assume``

*
  ``Contract_Cases``

*
  ``Debug``

*
  ``Invariant``

*
  ``Loop_Invariant``

*
  ``Loop_Variant``

*
  ``Postcondition``

*
  ``Precondition``

*
  ``Predicate``

*
  ``Refined_Post``



*definition of the following aspects*


*Language-defined*

*
  ``Static_Predicate``

*
  ``Dynamic_Predicate``

*
  ``Pre``

*
  ``Pre'Class``

*
  ``Post``

*
  ``Post'Class``

*
  ``Type_Invariant``

*
  ``Type_Invariant'Class``


*GNAT-specific*

*
  ``Contract_Cases``

*
  ``Invariant``

*
  ``Invariant'Class``

*
  ``Predicate``

*
  ``Refined_Post``




.. _Controlled_Type_Declarations:

``Controlled_Type_Declarations``
--------------------------------

.. index:: Controlled_Type_Declarations

Flag all declarations of controlled types. A declaration of a private type
is flagged if its full declaration declares a controlled type. A declaration
of a derived type is flagged if its ancestor type is controlled. Subtype
declarations are not checked. A declaration of a type that itself is not a
descendant of a type declared in ``Ada.Finalization`` but has a controlled
component is not checked.

This rule has no parameters.

.. _Declarations_In_Blocks:

``Declarations_In_Blocks``
--------------------------

.. index:: Declarations_In_Blocks

Flag all block statements containing local declarations. A ``declare``
block with an empty *declarative_part* or with a *declarative part*
containing only pragmas and/or ``use`` clauses is not flagged.

This rule has no parameters.

.. _Deeply_Nested_Inlining:

``Deeply_Nested_Inlining``
--------------------------

.. index:: Deeply_Nested_Inlining

Flag a subprogram (or generic subprogram) if pragma Inline has been applied
to it, and it calls another subprogram to which pragma Inline applies,
resulting in potential nested inlining, with a nesting depth exceeding the
value specified by the *N* rule parameter.

This rule requires the global analysis of all the compilation units that
are *gnatcheck* arguments; such analysis may affect the tool's
performance.

This rule has the following (mandatory) parameter for the ``+R`` option:



*N*
  Positive integer specifying the maximum level of nested calls to
  subprograms to which pragma Inline has been applied.

.. _Default_Parameters:

``Default_Parameters``
----------------------

.. index:: Default_Parameters

Flag all default expressions in parameters specifications. All parameter
specifications are checked: in subprograms (including formal, generic and
protected subprograms) and in task and protected entries (including accept
statements and entry bodies).

This rule has no parameters.

.. _Discriminated_Records:

``Discriminated_Records``
-------------------------

.. index:: Discriminated_Records

Flag all declarations of record types with discriminants. Only the
declarations of record and record extension types are checked. Incomplete,
formal, private, derived and private extension type declarations are not
checked. Task and protected type declarations also are not checked.

This rule has no parameters.

.. _Explicit_Full_Discrete_Ranges:

``Explicit_Full_Discrete_Ranges``
---------------------------------

.. index:: Explicit_Full_Discrete_Ranges

Flag each discrete range that has the form ``A'First .. A'Last``.

This rule has no parameters.

.. _Fixed_Equality_Checks:

``Fixed_Equality_Checks``
-------------------------

.. index:: Fixed_Equality_Checks

Flag all calls to the predefined equality operations for fixed-point types.
Both '``=``' and '``/=``' operations are checked.
User-defined equality operations are not flagged, nor are uses of operators
that are renamings of the predefined equality operations.
Also, the '``=``' and '``/=``' operations for floating-point types
are not flagged.

This rule has no parameters.

.. _Float_Equality_Checks:

``Float_Equality_Checks``
-------------------------

.. index:: Float_Equality_Checks

Flag all calls to the predefined equality operations for floating-point types.
Both '``=``' and '``/=``' operations are checked.
User-defined equality operations are not flagged, nor are uses of operators
that are renamings of the predefined equality operations.
Also, the '``=``' and '``/=``' operations for fixed-point types
are not flagged.

This rule has no parameters.

.. _Function_Style_Procedures:

``Function_Style_Procedures``
-----------------------------

.. index:: Function_Style_Procedures

Flag each procedure that can be rewritten as a function. A procedure can be
converted into a function if it has exactly one parameter of mode ``out``
and no parameters of mode ``in out``. Procedure declarations,
formal procedure declarations, and generic procedure declarations are always
checked. Procedure
bodies and body stubs are flagged only if they do not have corresponding
separate declarations. Procedure renamings and procedure instantiations are
not flagged.

If a procedure can be rewritten as a function, but its ``out`` parameter is
of a limited type, it is not flagged.

Protected procedures are not flagged. Null procedures also are not flagged.

This rule has no parameters.

.. _Generics_In_Subprograms:

``Generics_In_Subprograms``
---------------------------

.. index:: Generics_In_Subprograms

Flag each declaration of a generic unit in a subprogram. Generic
declarations in the bodies of generic subprograms are also flagged.
A generic unit nested in another generic unit is not flagged.
If a generic unit is
declared in a local package that is declared in a subprogram body, the
generic unit is flagged.

This rule has no parameters.

.. _Implicit_IN_Mode_Parameters:

``Implicit_IN_Mode_Parameters``
-------------------------------

.. index:: Implicit_IN_Mode_Parameters

Flag each occurrence of a formal parameter with an implicit ``in`` mode.
Note that ``access`` parameters, although they technically behave
like ``in`` parameters, are not flagged.

This rule has no parameters.

.. _Improperly_Located_Instantiations:

``Improperly_Located_Instantiations``
-------------------------------------

.. index:: Improperly_Located_Instantiations

Flag all generic instantiations in library-level package specs
(including library generic packages) and in all subprogram bodies.

Instantiations in task and entry bodies are not flagged. Instantiations in the
bodies of protected subprograms are flagged.

This rule has no parameters.

.. _Library_Level_Subprograms:

``Library_Level_Subprograms``
-----------------------------

.. index:: Library_Level_Subprograms

Flag all library-level subprograms (including generic
subprogram instantiations).

This rule has no parameters.


.. _Membership_Tests:

``Membership_Tests``
---------------------------

.. index:: Membership_Tests

Flag use of membership test expression.

This rule has the following (optional) parameters for the ``+R`` option:

*Multi_Alternative_Only*
  Flag only those membership test expressions that have more than one
  membership choice in the membership choice list.


*Except_Assertions*
  Do not flag a membership test expression if it is a subcomponent
  of the following constructs:



*argument of the following pragmas*


*Language-defined*

*
  ``Assert``


*GNAT-specific*

*
  ``Assert_And_Cut``

*
  ``Assume``

*
  ``Contract_Cases``

*
  ``Debug``

*
  ``Invariant``

*
  ``Loop_Invariant``

*
  ``Loop_Variant``

*
  ``Postcondition``

*
  ``Precondition``

*
  ``Predicate``

*
  ``Refined_Post``



*definition of the following aspects*


*Language-defined*

*
  ``Static_Predicate``

*
  ``Dynamic_Predicate``

*
  ``Pre``

*
  ``Pre'Class``

*
  ``Post``

*
  ``Post'Class``

*
  ``Type_Invariant``

*
  ``Type_Invariant'Class``


*GNAT-specific*

*
  ``Contract_Cases``

*
  ``Invariant``

*
  ``Invariant'Class``

*
  ``Predicate``

*
  ``Refined_Post``


These two parameters are independent on each other.


.. _Non_Qualified_Aggregates:

``Non_Qualified_Aggregates``
----------------------------

.. index:: Non_Qualified_Aggregates

Flag each non-qualified aggregate.
A non-qualified aggregate is an
aggregate that is not the expression of a qualified expression. A
string literal is not considered an aggregate, but an array
aggregate of a string type is considered as a normal aggregate.
Aggregates of anonymous array types are not flagged.

This rule has no parameters.

.. _Numeric_Literals:

``Numeric_Literals``
--------------------

.. index:: Numeric_Literals

Flag each use of a numeric literal in an index expression, and in any
circumstance except for the following:

*
  a literal occurring in the initialization expression for a constant
  declaration or a named number declaration, or

*
  a literal occurring in an aspect definition or in an aspect clause, or

*
  an integer literal that is less than or equal to a value
  specified by the *N* rule parameter.

*
  a literal occurring in a declaration in case the *Statements_Only*
  rule parameter is given

This rule may have the following parameters for the ``+R`` option:



*N*
  *N* is an integer literal used as the maximal value that is not flagged
  (i.e., integer literals not exceeding this value are allowed)


``ALL``
  All integer literals are flagged


``Statements_Only``
  Numeric literals are flagged only when used in statements

If no parameters are set, the maximum unflagged value is 1, and the check for
literals is not limited by statements only.

The last specified check limit (or the fact that there is no limit at
all) is used when multiple ``+R`` options appear.

The ``-R`` option for this rule has no parameters.
It disables the rule and restores its default operation mode.
If the ``+R`` option subsequently appears, will be 1, and the check will
not be limited by statements only.

.. _Parameters_Out_Of_Order:

``Parameters_Out_Of_Order``
---------------------------

.. index:: Parameters_Out_Of_Order

Flag each subprogram and entry declaration whose formal parameters are not
ordered according to the following scheme:


* ``in`` and ``access`` parameters first,
  then ``in out`` parameters,
  and then ``out`` parameters;

* for ``in`` mode, parameters with default initialization expressions
  occur last

Only the first violation of the described order is flagged.

The following constructs are checked:

* subprogram declarations (including null procedures);
* generic subprogram declarations;
* formal subprogram declarations;
* entry declarations;
* subprogram bodies and subprogram body stubs that do not
  have separate specifications

Subprogram renamings are not checked.

This rule has no parameters.


.. _Predicate_Testing:

``Predicate_Testing``
---------------------------

.. index:: Predicate_Testing

Flag a subtype mark if it denotes a subtype defined with (static or
dynamic) subtype predicate and is used as a membership choice in a
membership test expression.

Flags 'Valid attribute reference if the nominal subtype of the attribute
prefix has (static or dynamic) subtype predicate.


This rule has the following (optional) parameters for the ``+R`` option:



*Except_Assertions*
  Do not flag a construct described above if it is a subcomponent
  of the following constructs:



*argument of the following pragmas*


*Language-defined*

*
  ``Assert``


*GNAT-specific*

*
  ``Assert_And_Cut``

*
  ``Assume``

*
  ``Contract_Cases``

*
  ``Debug``

*
  ``Invariant``

*
  ``Loop_Invariant``

*
  ``Loop_Variant``

*
  ``Postcondition``

*
  ``Precondition``

*
  ``Predicate``

*
  ``Refined_Post``



*definition of the following aspects*


*Language-defined*

*
  ``Static_Predicate``

*
  ``Dynamic_Predicate``

*
  ``Pre``

*
  ``Pre'Class``

*
  ``Post``

*
  ``Post'Class``

*
  ``Type_Invariant``

*
  ``Type_Invariant'Class``


*GNAT-specific*

*
  ``Contract_Cases``

*
  ``Invariant``

*
  ``Invariant'Class``

*
  ``Predicate``

*
  ``Refined_Post``


.. _Quantified_Expressions:

``Quantified_Expressions``
--------------------------

.. index:: Quantified_Expressions

Flag use of quantified expression.

This rule has the following (optional) parameters for the ``+R`` option:



*Except_Assertions*
  Do not flag a conditional expression if it is a subcomponent
  of the following constructs:



*argument of the following pragmas*


*Language-defined*

*
  ``Assert``


*GNAT-specific*

*
  ``Assert_And_Cut``

*
  ``Assume``

*
  ``Contract_Cases``

*
  ``Debug``

*
  ``Invariant``

*
  ``Loop_Invariant``

*
  ``Loop_Variant``

*
  ``Postcondition``

*
  ``Precondition``

*
  ``Predicate``

*
  ``Refined_Post``



*definition of the following aspects*


*Language-defined*

*
  ``Static_Predicate``

*
  ``Dynamic_Predicate``

*
  ``Pre``

*
  ``Pre'Class``

*
  ``Post``

*
  ``Post'Class``

*
  ``Type_Invariant``

*
  ``Type_Invariant'Class``


*GNAT-specific*

*
  ``Contract_Cases``

*
  ``Invariant``

*
  ``Invariant'Class``

*
  ``Predicate``

*
  ``Refined_Post``




.. _Raising_Predefined_Exceptions:

``Raising_Predefined_Exceptions``
---------------------------------

.. index:: Raising_Predefined_Exceptions

Flag each ``raise`` statement that raises a predefined exception
(i.e., one of the exceptions ``Constraint_Error``, ``Numeric_Error``,
``Program_Error``, ``Storage_Error``, or ``Tasking_Error``).

This rule has no parameters.

.. _Unassigned_OUT_Parameters:

``Unassigned_OUT_Parameters``
-----------------------------

.. index:: Unassigned_OUT_Parameters

Flag procedures' ``out`` parameters that are not assigned.

An ``out`` parameter is flagged if the *sequence of statements* of
the procedure body (before the procedure body's exception part, if any)
contains no assignment to the parameter.

An ``out`` parameter is flagged in an *exception handler* in the exception
part of the procedure body, if the *exception handler* contains neither an
assignment to the parameter nor a raise statement.

Bodies of generic procedures are also considered.

The following are treated as assignments to an ``out`` parameter:

*
  an assignment statement, with the parameter or some component as the target

*
  passing the parameter (or one of its components) as an ``out`` or
  ``in out`` parameter, except for the case when it is passed to the
  call of an attribute subprogram.

This rule has no parameters.

.. warning:: This rule only detects a trivial case of an unassigned variable
   and doesn't provide a guarantee that there is no uninitialized access.
   The rule does not check function parameters (starting from Ada 2012 functions
   can have ``out`` parameters). It is not a replacement for rigorous check for
   uninitialized access provided by advanced static analysis tools.

.. _Unconstrained_Array_Returns:

``Unconstrained_Array_Returns``
-------------------------------

.. index:: Unconstrained_Array_Returns

Flag each function returning an unconstrained array. Function declarations,
function bodies (and body stubs) having no separate specifications,
and generic function instantiations are flagged.
Function calls and function renamings are
not flagged.

Generic function declarations, and function declarations in generic
packages, are not flagged.  Instead, this rule flags the results of
generic instantiations (that is, expanded specification and expanded
body corresponding to an instantiation).

This rule has the following (optional) parameters for the ``+R`` option:



*Except_String*
  Do not flag functions that return the predefined ``String`` type or a type
  derived from it, directly or indirectly.

Metrics-Related Rules
=====================

.. index:: Metrics-related rules

The rules in this section can be used to enforce compliance with
specific code metrics, by checking that the metrics computed for a program
lie within user-specifiable bounds.
Depending on the metric, there may be a lower bound, an upper bound, or both.
A construct is flagged if the value of the metric exceeds the upper bound
or is less than the lower bound.

The name of any metrics rule consists of the prefix ``Metrics_``
followed by the name of the corresponding metric:
``Essential_Complexity``, ``Cyclomatic_Complexity``, or
``LSLOC``.
(The 'LSLOC' acronym stands for 'Logical Source Lines Of Code'.)
The meaning and the computed values of the metrics are
the same as in *gnatmetric*.

For the ``+R`` option, each metrics rule has a numeric parameter
specifying the bound (integer or real, depending on a metric).
The ``-R``
option for the metrics rules does not have a parameter.

*Example:* the rule

::

  +RMetrics_Cyclomatic_Complexity : 7


means that all bodies with cyclomatic complexity exceeding 7 will be flagged.

To turn OFF the check for cyclomatic complexity metric,
use the following option:

::

  -RMetrics_Cyclomatic_Complexity


.. _Metrics_Essential_Complexity:

``Metrics_Essential_Complexity``
--------------------------------

.. index:: Metrics_Essential_Complexity

The ``Metrics_Essential_Complexity`` rule takes a positive integer as
upper bound.  A program unit that is an executable body exceeding this limit will be flagged.

The Ada essential complexity metric is a McCabe cyclomatic complexity metric counted
for the code that is reduced by excluding all the pure structural Ada control statements.

.. _Metrics_Cyclomatic_Complexity:

``Metrics_Cyclomatic_Complexity``
---------------------------------

.. index:: Metrics_Cyclomatic_Complexity

The ``Metrics_Cyclomatic_Complexity`` rule takes a positive integer as
upper bound.  A program unit that is an executable body exceeding this limit will be flagged.

The McCabe cyclomatic complexity metric is defined
in `http://www.mccabe.com/pdf/mccabe-nist235r.pdf <http://www.mccabe.com/pdf/mccabe-nist235r.pdf>`_
The goal of cyclomatic complexity metric is to estimate the number
of independent paths in the control flow graph that in turn gives the number
of tests needed to satisfy paths coverage testing completeness criterion.

.. _Metrics_LSLOC:

``Metrics_LSLOC``
-----------------

.. index:: Metrics_LSLOC

The ``Metrics_LSLOC`` rule takes a positive integer as
upper bound.  A program unit declaration or a program unit body exceeding
this limit will be flagged.

The metric counts the total number of declarations and the total number of statements.

SPARK Ada Rules
===============

.. index:: SPARK Ada related rules

The rules in this section can be used to enforce
compliance with the Ada subset allowed by the SPARK tools.

.. _Annotated_Comments:

``Annotated_Comments``
----------------------

.. index:: Annotated_Comments

Flags comments that are used as annotations or as
special sentinels/markers. Such comments have the following
structure


::

    --<special_character> <comment_marker>


where



*<special_character>*
  character (such as '#', '$', '|' etc.) indicating that the comment is used
  for a specific purpose


*<comment_marker>*
  a word identifying the annotation or special usage (word here is any sequence
  of characters except white space)

There may be any amount of white space (including none at all) between
``<special_character>`` and ``<comment_marker>``, but no white space
is permitted between ``'--'`` and ``<special_character>``. (A
white space here is either a space character or horizontal tabulation)

``<comment_marker>`` must not contain any white space.

``<comment_marker>`` may be empty, in which case the rule
flags each comment that starts with ``--<special_character>`` and
that does not contain any other character except white space

The rule has the following (mandatory) parameter for the ``+R`` option:



*S*
  String with the following interpretation: the first character
  is the special comment character, and the rest is
  the comment marker. S must not contain white space.

The ``-R`` option erases all definitions of special comment annotations
specified by the previous +R options.

The rule is case-sensitive.

Example:

The rule


::

  +RAnnotated_Comments:#hide


will flag the following comment lines


.. code-block:: ada

  --#hide
  --# hide
  --#           hide

     I := I + 1; --# hide


But the line


.. code-block:: ada

  -- # hide


will not be flagged, because of the space between '--' and '#'.

The line


.. code-block:: ada

  --#Hide


will not be flagged, because the string parameter is case sensitive.

.. _Boolean_Relational_Operators:

``Boolean_Relational_Operators``
--------------------------------

.. index:: Boolean_Relational_Operators

Flag each call to a predefined relational operator ('<', '>', '<=',
'>=', '=' and '/=') for the predefined Boolean type.
(This rule is useful in enforcing the SPARK language restrictions.)

Calls to predefined relational operators of any type derived from
``Standard.Boolean`` are not detected.  Calls to user-defined functions
with these designators, and uses of operators that are renamings
of the predefined relational operators for ``Standard.Boolean``,
are likewise not detected.

This rule has no parameters.

.. _Expanded_Loop_Exit_Names:

``Expanded_Loop_Exit_Names``
----------------------------

.. index:: Expanded_Loop_Exit_Names

Flag all expanded loop names in ``exit`` statements.

This rule has no parameters.

.. _Non_SPARK_Attributes:

``Non_SPARK_Attributes``
------------------------

.. index:: Non_SPARK_Attributes

The SPARK language defines the following subset of Ada 95 attribute
designators as those that can be used in SPARK programs. The use of
any other attribute is flagged.

* ``'Adjacent``
* ``'Aft``
* ``'Base``
* ``'Ceiling``
* ``'Component_Size``
* ``'Compose``
* ``'Copy_Sign``
* ``'Delta``
* ``'Denorm``
* ``'Digits``
* ``'Exponent``
* ``'First``
* ``'Floor``
* ``'Fore``
* ``'Fraction``
* ``'Last``
* ``'Leading_Part``
* ``'Length``
* ``'Machine``
* ``'Machine_Emax``
* ``'Machine_Emin``
* ``'Machine_Mantissa``
* ``'Machine_Overflows``
* ``'Machine_Radix``
* ``'Machine_Rounds``
* ``'Max``
* ``'Min``
* ``'Model``
* ``'Model_Emin``
* ``'Model_Epsilon``
* ``'Model_Mantissa``
* ``'Model_Small``
* ``'Modulus``
* ``'Pos``
* ``'Pred``
* ``'Range``
* ``'Remainder``
* ``'Rounding``
* ``'Safe_First``
* ``'Safe_Last``
* ``'Scaling``
* ``'Signed_Zeros``
* ``'Size``
* ``'Small``
* ``'Succ``
* ``'Truncation``
* ``'Unbiased_Rounding``
* ``'Val``
* ``'Valid``

This rule has no parameters.

.. _Non_Tagged_Derived_Types:

``Non_Tagged_Derived_Types``
----------------------------

.. index:: Non_Tagged_Derived_Types

Flag all derived type declarations that do not have a record extension part.

This rule has no parameters.

.. _Outer_Loop_Exits:

``Outer_Loop_Exits``
--------------------

.. index:: Outer_Loop_Exits

Flag each ``exit`` statement containing a loop name that is not the name
of the immediately enclosing ``loop`` statement.

This rule has no parameters.

.. _Overloaded_Operators:

``Overloaded_Operators``
------------------------

.. index:: Overloaded_Operators

Flag each function declaration that overloads an operator symbol.
A function body is checked only if the body does not have a
separate spec. Formal functions are also checked. For a
renaming declaration, only renaming-as-declaration is checked

This rule has no parameters.

.. _Slices:

``Slices``
----------

.. index:: Slices

Flag all uses of array slicing

This rule has no parameters.

.. _Universal_Ranges:

``Universal_Ranges``
--------------------

.. index:: Universal_Ranges rule

Flag discrete ranges that are a part of an index constraint, constrained
array definition, or ``for``-loop parameter specification, and whose bounds
are both of type *universal_integer*. Ranges that have at least one
bound of a specific type (such as ``1 .. N``, where ``N`` is a variable
or an expression of non-universal type) are not flagged.

This rule has no parameters.
