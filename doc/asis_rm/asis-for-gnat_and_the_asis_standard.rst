.. _ASIS-for-GNAT_and_the_ASIS_Standard:

***********************************
ASIS-for-GNAT and the ASIS Standard
***********************************

ASIS-for-GNAT implements ASIS 95
and contains several extensions
(see :ref:`ASIS_Extensions`) as allowed by the ASIS Standard,
Section 1.1.3.1.

The differences between the GNAT and standard ASIS are that ASIS-for-GNAT:

*
  includes GNAT-specific comment headers at the beginning of each source file;

*
  supplies additional context clauses;

*
  defines the packages' private parts;

*
  is formatted to comply with GNAT coding style;

*
  declares the ``Is_Dispatching_Operation`` query
  in ``Asis.Declarations``
  rather than in ``Asis.Expressions``.

  .. index:: Is_Dispatching_Operation query
  .. index:: Asis.Declarations package
  .. index:: Asis.Expressions package

  This query has ``A_Declaration`` ``Element`` as
  its argument and, according to the general principles of the ASIS package
  hierarchy, it should be in the ``Asis.Declarations`` spec;

*
  for the optional Data Decomposition Annex, the package
  ``Asis.Data_Decomposition.Portable_Transfer`` is not provided;

*
  includes extensions that support features introduced in Ada 2005.
