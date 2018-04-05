.. _How_to_Build_Efficient_ASIS_Applications:

****************************************
How to Build Efficient ASIS Applications
****************************************

.. index:: ASIS Performance

This chapter identifies some potential performance issues with ASIS applications
and offers some advice on how to address these issues.

Tree Swapping as a Performance Issue
====================================

.. index:: Tree swapping (ASIS performance issue)

If an ASIS ``Context`` comprises more then one tree, then ASIS may need to switch
between different trees during an ASIS application run. Switching between
trees may require ASIS to repeatedly read in the same set of trees, and this may slow
down an application considerably.

Basically, there are two causes for tree swapping:

*
  *Processing of semantically independent units.* Suppose in ``Context`` ``Cont`` we have
  units ``P`` and ``Q`` that do not depend on each other, and ``Cont`` does
  not contain any third unit depending on both ``P`` and ``Q``. This
  means that ``P`` and ``Q`` cannot be represented by the same tree. To
  obtain information about ``P``, ASIS needs to access the tree :file:`p.adt`,
  and to get some information about ``Q``, ASIS needs
  :file:`q.adt`. Therefore, if an application retrieves some information from
  ``P``, and then starts processing ``Q``, ASIS has to read
  :file:`q.adt`.

*
  *Processing of information from dependent units.*
  A unit ``U`` may be present not only in the tree created for ``U``, but also in
  all the trees created for units which semantically depend upon ``U``.
  Suppose we have a library procedure ``Proc`` depending on a
  library package ``Pack``, and in the set of trees making up our ``Context`` we
  have trees :file:`pack.adt` and :file:`proc.adt`. Suppose we have some
  ``Element`` representing a component of ``Pack``, when :file:`pack.adt` was
  accessed by ASIS, and suppose that because of some other actions undertaken
  by an application ASIS changed the tree being accessed to :file:`proc.adt`.
  Suppose that now the application wants to do something with the ``Element``
  representing some component of ``Pack`` and obtained from :file:`pack.adt`. Even
  though the unit ``Pack`` is represented by the currently accessed tree
  :file:`proc.adt`, ASIS has to switch back to :file:`pack.adt`, because all the
  references into the tree structure kept as a part of the value of this
  ``Element`` are valid only for :file:`pack.adt`.

Queries That Can Cause Tree Swapping
====================================

In ASIS-for-GNAT, tree swapping can currently take place only when
processing queries defined in:


.. code-block:: ada

  Asis.Elements
  Asis.Declarations
  Asis.Definitions
  Asis.Statements
  Asis.Clauses
  Asis.Expressions
  Asis.Text


but not for those queries in the above packages that return enumeration or boolean results.

.. index:: Asis.Iterator.Traverse_Element generic procedure

For any instantiation of ``Asis.Iterator.Traverse_Element``,
the traversal itself
can cause at most one tree read to get the tree appropriate for processing the
``Element`` to be traversed, but procedures provided as actuals for
``Pre_Operation`` and ``Post_Operation`` may cause additional tree
swappings.

How to Avoid Unnecessary Tree Swapping
======================================

.. index:: Tree swapping (ASIS performance issue)

To speed up your application, try to avoid unnecessary tree swapping. The
following guidelines may help:

*
  Try to minimize the set of tree files processed by your application. In
  particular, try to avoid having separate trees created for subunits.

  Minimizing the set of tree files processed by the application also cuts
  down the time needed for opening a ``Context``. Try to use ``gnatmake`` to create
  a suitable set of tree files covering an Ada program for processing by
  an ASIS application.

*
  Choose the ``Context`` definition appropriate to your application. For
  example, use 'one tree' ``Context`` (``-C1``) for applications that are limited
  to processing single units (such as a pretty printer or ``gnatstub``). By
  processing the tree file created for this unit, ASIS can get all the
  syntactic and semantic information about this unit. Using the 'one tree' ``Context``
  definition, an application has only one tree file to read when
  opening a ``Context``, and no other tree file will be read during the
  application run. An 'N-trees' ``Context`` is a natural extension of 'one tree'
  ``Context`` for applications that know in advance which units will be
  processed, but opening a ``Context`` takes longer, and ASIS may switch among
  different tree files during an application run. Use 'all trees' ``Context``
  only for applications which are not targeted at processing a specific
  unit or a specific set of units, but are supposed to process all the
  available units, or when an application has to process a large
  system consisting of a many units. When using an
  application based on an 'all trees' ``Context``, use the approach for creating
  tree files described above to minimize a set of tree files to be
  processed.

*
  In your ASIS application, try to avoid switching between processing units or
  sets of units with no dependencies among them; such a switching will
  cause tree swapping.

*
  If you are going to analyze a library unit having both a spec and a body,
  start by obtaining an ``Element`` from the body of this unit. This will set
  the tree created for the body as the tree accessed by ASIS, and this tree
  will allow both the spec and the body of this unit to be processed
  without tree swapping.

  .. index:: Asis.Implementation.Initialize procedure

*
  To see a 'tree swapping profile' of your application use the ``-dt`` debug flag
  when initializing ASIS (``Asis.Implementation.Initialize ("-dt")``).
  The
  information returned may give you some hints on
  how to avoid tree swapping.

.. _Using_gnatmake_to_Create_Tree_Files:

Using ``gnatmake`` to Create Tree Files
=======================================

.. index:: gnatmake (for creating tree files)

.. index:: -gnatct option

To create a suitable set of tree files, you may use ``gnatmake``. GNAT
creates an :file:`ALI` file for every successful compilation, whether or not
code has been generated. Therefore, it is possible to run ``gnatmake`` with
the ``-gnatct`` option;
this will create the set of
tree files for all the compilation units needed in the resulting program.
Below we will use
``gnatmake`` to create a set of tree files for a complete Ada program
(partition). You may adapt this approach to an incomplete program or to a
partition without a main subprogram, applying ``gnatmake`` to some of its
components.

Using ``gnatmake`` for creating tree files has another advantage: it will
keep tree files consistent among themselves and with the sources.

There are two different ways to use ``gnatmake`` to create a set of tree
files.

First, suppose you have object, :file:`ALI` and tree files for your program in the same
directory, and :file:`main_subprogram.adb` contains the body of the main
subprogram. If you run ``gnatmake`` as


::

  $ gnatmake -f -c -gnatct ... main_subprogram.adb


this will create the trees representing the full program for which
``main_subprogram`` is the main procedure. The trees will be created 'from scratch';
that is, if some tree files already exist, they will be recreated. This is
because ``gnatmake`` is being called with the ``-f`` option
(which means 'force recompilation').
Usng ``gnatmake`` without the ``-f`` option for creating tree files is not reliable
if your tree files are in the same directory as the object files, because
object and tree files 'share' the same set of :file:`ALI` files.
If the
object files exist and are consistent with the :file:`ALI` and source
files, the source will not be recompiled for creating a tree file unless the ``-f``
option is set.

A different approach is to combine the tree files and the associated :file:`ALI` files
in a separate directory, and to use this directory only for keeping the tree
files and maintaining their consistency with source files. Thus, the object
files and their associated :file:`ALI` files should be in another directory.
In this case, by invoking ``gnatmake`` through:


::

  $ gnatmake -c -gnatct ... main_subprogram.adb


(that is, without forcing recompilation) you will still obtain a full and
consistent set of tree files representing your program, but in this case the
existing tree files will be reused.

See the next chapter for specific details related to Ada compilation units
belonging to precompiled Ada libraries.
