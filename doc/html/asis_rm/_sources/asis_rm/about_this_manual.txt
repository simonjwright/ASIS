About This Manual
~~~~~~~~~~~~~~~~~

This Manual contains reference material for developers
using ASIS-for-GNAT --- GNAT's implementation of
the Ada Semantic Interface Specification (ASIS).
It provides information about ASIS-for-GNAT's implementation-specific
characteristics and
current implementation limitations.
(The term 'implementation-specific' in ASIS means what is
called 'implementation-defined' in the Ada Reference Manual.)

ASIS has been designed as a portable basis for many kinds of Ada code
analysis tools. However, for situations where a developer may need to
exploit the characteristics of a particular Ada compiler,
ASIS also contains a number of implementation-specific
features.  These allow interfacing with the underlying Ada implementation, as
well as exploiting the implementation permissions for particular queries.

Of course, any ASIS application that uses implementation-specific features
may be nonportable. You should follow good programming practice and isolate
and clearly document any sections of your program that make use of such
features in a nonportable manner.

**What This Manual Contains**

This manual contains the following chapters:

*
  :ref:`ASIS-for-GNAT_and_the_ASIS_Standard`, describes the relationship
  between ASIS-for-GNAT and the existing ASIS International Standard.

*
  :ref:`ASIS_Extensions`, describes the contents of the packages
  ``Asis.Extensions``, ``Asis.Extensions.Flat_Kinds`` and
  ``Asis.Extensions.Iterator``.

*
  :ref:`Implementation-Specific_Features_and_Implementation_Permissions`,
  presents the aspects of the ASIS definition that are
  implementation specific and describes their treatment in ASIS-for-GNAT.

*
  :ref:`Debugging_Information`, describes the kinds of debugging information that
  you can generate with ASIS-for-GNAT.

**What You Should Know Before Reading This Manual**

This Reference Manual assumes that you are familiar with Ada 95 language as
defined by the International Standard ISO/IEC-8652:1995, and
with ASIS 95 as defined by the
ASIS 95 International Standard ISO/IEC 15291:1999.

This Manual supplements the information presented in the
ASIS-for-GNAT User's Guide and uses the terminology introduced there.

**Related Information**

For more information, please refer to the following documents:

*
  GNAT User's Guide

*
  ASIS-for-GNAT User's Guide

*
  Ada 95 Reference Manual

*
  ASIS 95 Standard
