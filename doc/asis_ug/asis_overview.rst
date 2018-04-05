.. _ASIS_Overview:

*************
ASIS Overview
*************

.. index:: ASIS overview

This chapter contains a short overview of the ASIS definition as given in
the ISO/IEC 15291:1999 ASIS Standard. This overview is aimed at helping an ASIS
newcomer find needed information in the ASIS definition.

For more details, please refer to the ASIS definition itself. To gain some initial
experience with ASIS, try the examples in :ref:`ASIS_Tutorials`.

Main ASIS Abstractions
======================

ASIS is based on three main abstractions used to describe Ada programs;
these abstractions are implemented as Ada private types:



*Context*

  .. index:: Context type

  An ASIS ``Context`` is a logical handle to an Ada environment, as defined in the
  Ada Reference Manual,
  Chapter 10. An ASIS application developer may view an ASIS ``Context`` as a way
  to define a set of compilation units available through the ASIS queries.

  .. index:: ASIS queries


*Compilation_Unit*

  .. index:: Compilation_Unit type

  An ASIS ``Compilation_Unit`` is a logical handle to an Ada compilation unit. It
  reflects practically all the properties of compilation units
  defined by the Ada Reference Manual,
  and it also reflects some properties of 'physical objects'
  used by an underlying Ada implementation to model compilation units.  Examples of
  such properties are the time of the last update, and
  the name of the object containing the unit's source text.
  An ASIS ``Compilation_Unit`` provides the 'black-box' view of a
  compilation unit, considering the unit as a whole. It may be decomposed
  into ASIS ``Element``\ s
  and then analyzed in 'white-box' fashion.


  .. index:: Element type

*Element*

  .. index:: Element type

  An ASIS ``Element`` is a logical handle to a syntactic component of an ASIS
  ``Compilation_Unit`` (either explicit or implicit).

Some ASIS components use additional abstractions (private types) needed for
specific pieces of functionality:



*Container*

  .. index:: Container type

  .. index:: Asis.Ada_Environments.Containers package

  An ASIS ``Container`` (defined by the
  ``Asis.Ada_Environments.Containers`` package)
  provides a means for
  structuring the content of an ASIS ``Context``; i.e., ASIS ``Compilation_Unit``\ s
  are grouped into ``Container``\ s.


*Line*

  .. index:: Line type

  .. index:: Asis.Text package

  An ASIS ``Line`` (defined by the ``Asis.Text`` package)
  is the
  abstraction of a line of code in an Ada source text. An ASIS ``Line`` has a length, a
  string image and a number.


*Span*

  .. index:: Span type

  .. index:: Asis.Text package

  An ASIS ``Span`` (defined by the ``Asis.Text`` package)
  defines the
  location of an ``Element``, a ``Compilation_Unit``, or a whole compilation in the
  corresponding source text.


*Id*

  .. index:: Id type

  .. index:: Asis.Ids package

  An ASIS ``Id`` (defined by the ``Asis.Ids``
  package)  provides a way to
  store some 'image' of an ASIS ``Element`` outside an ASIS application. An
  application may create an ``Id`` value from an ``Element`` and store it in a
  file. Subsequently the same or another application may read this ``Id`` value
  and convert it back into the corresponding ``Element`` value.

.. _ASIS_Package_Hierarchy:

ASIS Package Hierarchy
======================

.. index:: ASIS package hierarchy

ASIS is defined as a hierarchy of Ada packages. Below is a
short description of this hierarchy.



``Asis``

  .. index:: Asis package

  .. index:: Context type

  .. index:: Compilation_Unit type

  .. index:: Element type

  The root package of the hierarchy. It defines the main ASIS
  abstractions --- ``Context``,
  ``Compilation_Unit``
  and ``Element``
  --- as Ada private types. It also contains a set of enumeration types that define
  the classification hierarchy for ASIS ``Element``\ s (which closely reflects the
  Ada syntax defined in the Ada Reference Manual) and the classification of
  ASIS ``Compilation_Unit``\ s.
  This package does not contain any queries.


``Asis.Implementation``
  Contains subprograms that control an ASIS implementation: initializing and
  finalizing it, retrieving and resetting diagnosis information. Its child
  package ``Asis.Implementation.Permissions``
  contains boolean queries that
  reflect how ASIS implementation-specific features are implemented.

  .. index:: Asis.Implementation package

  .. index:: Asis.Implementation.Permissions package

``Asis.Ada_Environments``

  .. index:: Asis.Ada_Environments package

  .. index:: ASIS queries

  .. index:: Context type

  Contains queries
  that deal with an ASIS ``Context``: associating and dissociating,
  opening and closing a ``Context``.


``Asis.Compilation_Units``

  .. index:: Asis.Compilation_Units package

  .. index:: ASIS queries

  Contains queries
  that work with ASIS ``Compilation_Unit``\ s: obtaining units from a
  ``Context``, getting semantic dependencies between units and 'black-box' unit
  properties.


``Asis.Compilation_Units.Relations``

  .. index:: Asis.Compilation_Units.Relations package

  .. index:: ASIS queries

  Contains queries
  that return integrated semantic dependencies among ASIS
  ``Compilation_Unit``\ s; e.g., all the units needed by a given unit to be included
  in a partition.


``Asis.Elements``

  .. index:: Asis.Elements package

  .. index:: ASIS queries

  Contains queries
  working on ``Element``\ s and implementing general ``Element``
  properties: gateway queries from ASIS Compilation Units to ASIS ``Element``\ s,
  queries defining the position of an ``Element`` in the ``Element`` classification
  hierarchy, queries which define for a given ``Element`` its enclosing
  ``Compilation_Unit`` and its enclosing ``Element``.
  It also contains queries for processing pragmas.


*Packages working on specific* ``Element``\ s

  .. index:: Asis.Declarations package

  .. index:: Asis.Definitions package

  .. index:: Asis.Statements package

  .. index:: Asis.Expressions package

  .. index:: ASIS.Clauses package

  This group contains the following packages: ``Asis.Declarations``,
  ``Asis.Definitions``,
  ``Asis.Statements``,
  ``Asis.Expressions``
  and
  ``ASIS.Clauses``.
  Each of these packages contains queries working on
  ``Element``\ s of the corresponding kind --- that is, representing Ada declarations,
  definitions, statements, expressions and clauses respectively.


``Asis.Text``

  .. index:: Asis.Text package

  .. index:: ASIS queries

  Contains queries
  returning information about the source representation of ASIS
  ``Compilation_Unit``\ s and ASIS ``Element``\ s.


``Asis.Exceptions``

  .. index:: Asis.Exceptions package

  Defines ASIS exceptions.


``Asis.Errors``

  .. index:: Asis.Errors package

  Defines possible ASIS error status values.

Structural and Semantic Queries
===============================

.. index:: ASIS queries
.. index:: Structural ASIS queries

.. index:: Semantic ASIS queries

.. index:: Enclosing_Element query

.. index:: Asis.Elements.Enclosing_Element query

Queries working on ``Element``\ s and returning ``Element``\ s or ``Element`` lists
are divided into structural and semantic queries.
Each structural query (except ``Enclosing_Element``)
implements one step of
the parent-to-child decomposition of an Ada program according to the ASIS
``Element`` classification hierarchy. ``Asis.Elements.Enclosing_Element`` query
implements the reverse child-to-parent step. (For implicit ``Element``\ s obtained
as results of semantic queries, ``Enclosing_Element`` might not correspond to what
could be expected from the Ada syntax and semantics; in
this case the documentation of a semantic query also defines the effect of
``Enclosing_Element`` applied to its result).

A semantic query for a given ``Element`` returns the ``Element`` or the list of
``Element``\ s representing some semantic property --- e.g., a type
declaration for an expression as the expression's type, a defining identifier as a
definition for a simple name, etc.

For example, if we have ``Element`` ``El`` representing an assignment statement:


.. code-block:: ada

      X := A + B;


then we can retrieve the structural components of this assignment statement by
applying the appropriate structural queries:


.. code-block:: ada

     El_Var  := Asis.Statements.Assignment_Variable_Name (El); --  X
     El_Expr := Asis.Statements.Assignment_Expression    (El); --  A + B


Then we can analyze semantic properties of the variable name represented by
``El_Var`` and of the expression represented by ``El_Expr`` by means of
appropriate semantic queries:


.. code-block:: ada

     El_Var_Def   :=
        Asis.Expressions.Corresponding_Name_Definition (El_Var);
     El_Expt_Type :=
        Asis.Expressions.Corresponding_Expression_Type (El_Expr);


As a result, ``El_Var_Def`` will be of ``A_Defining_Identifier`` kind
and will represent the defining occurrence of ``X``, while
``El_Expt_Type`` of a kind ``An_Ordinary_Type_Declaration`` will
represent the declaration of the type of the expression ``A + B``.

If we apply ``Asis.Elements.Enclosing_Element`` to ``El_Var`` or to
``El_Expr``, we will get back to the ``Element`` representing the
assignment statement.

An important difference between classifying queries working on ``Element``\ s as
structural versus
semantic is that all the structural queries must be within one ASIS
``Compilation_Unit``, but for semantic queries it is typical for the
argument of a query to be in one ASIS ``Compilation_Unit``, while the result of this
query is in another ASIS ``Compilation_Unit``.

ASIS Error Handling Policy
==========================

.. index:: Error Handling

.. index:: Storage_Error (propagated from ASIS queries)

.. index:: Asis.Exceptions package

.. index:: Asis.Errors.Error_Kinds type

.. index:: Diagnosis string

.. index:: Asis.Implementation.Status query

.. index:: Asis.Implementation.Diagnosis query

.. index:: Asis.Implementation.Set_Status procedure

Only ASIS-defined exceptions (and the Ada predefined ``Storage_Error``
exception) propagate out from ASIS queries. ASIS exceptions
are defined in the ``Asis.Exceptions`` package.
When an ASIS exception is raised, ASIS sets the Error Status (the possible
ASIS error conditions are defined as the values of the
``Asis.Errors.Error_Kinds`` type)
and forms the ``Diagnosis`` string.
An application can query the current value of the ASIS Error Status by the
``Asis.Implementation.Status`` query,
and the current content of the
``Diagnosis`` string by ``Asis.Implementation.Diagnosis`` query.
An application
can reset the Error Status and the ``Diagnosis`` string by
invoking the ``Asis.Implementation.Set_Status`` procedure.

.. index:: Tasking and error information

*Caution:* The ASIS way of providing error information is not tasking safe.
The ``Diagnosis`` string and Error Kind are global to an entire partition,
and are not 'per task'.
If ASIS exceptions are raised in more then
one task of a multi-tasking ASIS application, the result of
querying the error information in a particular task may be incorrect.

Dynamic Typing of ASIS Queries
==============================

.. index:: ASIS queries (dynamic typing)

.. index:: Element type

.. index:: Compilation_Unit type

.. index:: Asis.Elements package

The ASIS type ``Element``
covers all Ada syntactic constructs,
and ``Compilation_Unit``
covers all Ada compilation
units. ASIS defines an ``Element`` classification hierarchy (which reflects
very closely the hierarchy of Ada syntactic categories defined in the
Ada Reference Manual,
and ASIS similarly defines a classification scheme for ASIS ``Compilation_Unit``\ s.
For
any ``Element`` you can get its position in the ``Element``
classification hierarchy by means of classification queries defined in the
package ``Asis.Elements``.

.. index:: Asis.Compilation_Units package

The classification queries for ``Compilation_Unit``\ s
are defined in the package ``Asis.Compilation_Units``.
Many of the queries working on ``Element``\ s and ``Compilation_Unit``\ s can be applied
only to specific kinds of ``Element``\ s and ``Compilation_Unit``\ s respectively. For
example, it does not make sense to query
``Assignment_Variable_Name`` for an ``Element`` of
``An_Ordinary_Type_Declaration`` kind.
An attempt to perform such an operation will be detected at run-time, and
an exception will be raised as explained in the next paragraph.

.. index:: Asis.Exceptions.ASIS_Inappropriate_Element exception

.. index:: Asis.Errors.Value_Error error status

ASIS may be viewed as a dynamically typed interface. For any ``Element`` structural
or semantic query (that is, for a query having an ``Element`` as an argument and
returning either an ``Element`` or ``Element`` list as a result) a list of appropriate
``Element`` kinds is explicitly defined in the query documentation which
immediately follows the declaration of the corresponding subprogram in the
code of the ASIS package. This means that the query can be applied only to
argument ``Element``\ s being of the kinds from this list. If the kind of the
argument ``Element`` does not belong to this list, the corresponding call to this
query raises the ``Asis.Exceptions.ASIS_Inappropriate_Element`` exception
with ``Asis.Errors.Value_Error`` error status set.

.. index:: Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit exception

The situation for the queries working on ``Compilation_Unit``\ s is similar. If a
query lists appropriate unit kinds in its documentation, then this query can
work only on ``Compilation_Unit``\ s of the kinds from this list. The query should
raise ``Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit``
with ``Asis.Errors.Value_Error`` error status set when called for any
``Compilation_Unit`` with a kind not from the list of the appropriate unit kinds.

.. index:: Asis.Elements.Statement_Kind query

If a query has a list of expected ``Element`` kinds or expected ``Compilation_Unit``
kinds in its documentation, this query does not raise any exception when
called with any argument, but it produces a meaningful result only when called
with an argument with the kind from this list. For example, if
``Asis.Elements.Statement_Kind`` query
is called for an argument of
``A_Declaration`` kind, it just returns ``Not_A_Statement``, but without
raising any exception.

ASIS Iterator
=============

.. index:: ASIS Iterator

.. index:: Asis.Iterator.Traverse_Element generic procedure

ASIS provides a powerful mechanism to traverse an Ada unit, the generic
procedure ``Asis.Iterator.Traverse_Element``.
This procedure makes a top-down
left-to-right (or depth-first) traversal of the ASIS tree (that is, of
the syntax structure of the Ada code viewed as the hierarchy of ASIS
``Element``\ s). In the course of this traversal, it applies to each ``Element`` the
formal ``Pre_Operation`` procedure when visiting this ``Element`` for the first
time, and the formal ``Post_Operation`` procedure when leaving this ``Element``.
By providing specific procedures for ``Pre_Operation`` and
``Post_Operation`` when instantiating the generic unit, you
can automatically process all ASIS ``Element``\ s found
in a given ASIS tree.

For example, suppose we have an assignment statement:


.. code-block:: ada

      X := F (Y);


When called for an ``Element`` representing this statement, a
``Traverse_Element`` instantiation does the following (below ``Pre_Op``
and ``Post_Op`` stand for actual procedures provided for formal
``Pre_Operation`` and ``Post_Operation``, and numbers indicate the
sequence of calls to ``Pre_Op`` and ``Post_Op`` during traversal):


::

               (1 Pre_Op)  X := F (Y) (10 Post_Op)
                               |
                               |
             -----------------------------------
             |                                 |
  (2 Pre_Op) X (3 Post_Op)                     |
                                               |
                                  (4 Pre_Op) F(Y) (9 Post_Op)
                                               |
                                               |
                                  ---------------------------
                                  |                         |
                      (5 Pre_Op)  F (6 Post_Op)  (7 Pre_Op) Y (8 Post_Op)


To see in more detail how ``Traverse_Element`` may be used for rapid
development of a number of useful ASIS applications, try the examples in
:ref:`ASIS_Tutorials`.

How to Navigate through the ``Asis`` Package Hierarchy
======================================================

The following hints and tips may be useful when looking for some specific
information in the ASIS source files:

*
  Use the short overview of the ASIS packages given in
  :ref:`ASIS_Package_Hierarchy`, to limit your browsing to a smaller set
  of ASIS packages (e.g., if
  you are interested in what can be done with ``Compilation_Unit``\ s then look only in
  ``Asis.Compilation_Units``; if you are looking for queries that can be
  used to decompose and analyze declarations, limit your search to
  ``Asis.Declarations``).

*
  Inside ASIS packages working with particular kinds of ``Element``\ s
  (``Asis.Declarations``, ``Asis.Definitions``, ``Asis.Statements``,
  ``Asis.Expressions`` and ``ASIS.Clauses``) queries are ordered according
  to the order of the description of the corresponding constructions in the
  Ada Reference Manual
  (e.g., package ``Asis.Statements`` starts from a query retrieving labels
  and ends with the query decomposing a code statement).

*
  The names of all the semantic queries (and only ones) start from
  ``Corresponding_...`` or ``Implicit_...``

*
  Use comment sentinels given in the specification of the ASIS packages. A
  sentinel of the form '``--|ER``' (from '``Element`` Reference') introduces a new
  ``Element`` kind, and it is followed by a group of sentinels of the form
  '``--|CR``' (from 'Child Reference'), which list queries yielding the child
  ``Element``\ s for the ``Element`` just introduced.
