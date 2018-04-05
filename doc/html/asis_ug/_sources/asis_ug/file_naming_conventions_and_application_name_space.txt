.. _File_Naming_Conventions_and_Application_Name_Space:

**************************************************
File Naming Conventions and Application Name Space
**************************************************

Any ASIS application depends on the ASIS
interface components; an ASIS application programmer thus needs to be alert to (and to avoid)
clashes with the names of these components.

.. index:: Asis package

.. index:: Asis.Extensions package

.. index:: Asis.Set_Get package

.. index:: Asis.Text.Set_Get package

ASIS-for-GNAT includes the full specification of the ASIS Standard,
and also adds the following children and grandchildren of the root ``Asis`` package:

*
  ``Asis.Extensions`` hierarchy (the source file names start with
  :file:`asis-extensions`) defines some useful ASIS extensions, see ASIS
  Reference Manual for more details.
*
  ``Asis.Set_Get`` (the source files :file:`asis-set_get.ad(b|s)`
  respectively) contains the access and update subprograms for the
  implementation of the main ASIS abstractions defined in ``Asis``.
*
  ``Asis.Text.Set_Get`` (the source files :file:`asis-text-set_get.ad(b|s)`
  respectively) contains the access and update subprograms for the
  implementation of the ASIS abstractions defined in ``Asis.Text``;

.. index:: A4G package

All other ASIS-for-GNAT Ada implementation components belong to the
hierarchy rooted at the package ``A4G``
(which comes from 'ASIS-for-GNAT').

ASIS-for-GNAT also incorporates the following GNAT components as a part of the
ASIS implementation:


.. code-block:: ada

     Alloc
     Atree
     Casing
     Csets
     Debug
     Einfo
     Elists
     Fname
     Gnatvsn
     Hostparm
     Krunch
     Lib
       Lib.List
       Lib.Sort
     Namet
     Nlists
     Opt
     Output
     Repinfo
     Scans
     Sinfo
     Sinput
     Snames
     Stand
     Stringt
     Table
     Tree_In
     Tree_Io
     Types
     Uintp
     Uname
     Urealp
     Widechar


Therefore, in your ASIS application you should not add children at any level of the ``Asis``
or ``A4G`` hierarchies, and you should avoid using
any name from the list of the GNAT component names above.

All Ada source files making up the ASIS implementation for GNAT (including
the GNAT components being a part of ASIS-for-GNAT) follow the GNAT file name
conventions without any name 'krunch'ing.
