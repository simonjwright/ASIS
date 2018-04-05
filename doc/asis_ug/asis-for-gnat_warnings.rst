.. _ASIS-for-GNAT_Warnings:

**********************
ASIS-for-GNAT Warnings
**********************

.. index:: Warnings (from ASIS-for-GNAT)

The ASIS definition specifies the situations when certain ASIS-defined
exceptions should be raised, and ASIS-for-GNAT conforms to these rules.

ASIS-for-GNAT also generates warnings if it considers some situation arising
during the ASIS query processing to be potentially wrong, and if the
ASIS definition does not require raising an exception. Usually
this occurs with actual or potential problems in an
implementation-specific part of ASIS, such as providing
implementation-specific parameters to the queries ``Initialize``,
``Finalize`` and ``Associate`` or opening a ``Context``.

There are three warning modes in ASIS-for-GNAT:



*default*
  Warning messages are output to ``Standard_Error``.


*suppress*
  Warning messages are suppressed.


  .. index:: Asis_Failed exception

  .. index:: Diagnosis string

*treat as error*
  A warning is treated as an error by ASIS-for-GNAT: instead of sending a
  message to ``Standard_Error``, ASIS-for-GNAT raises ``Asis_Failed``
  and converts the
  warning message into the ASIS ``Diagnosis`` string.
  ASIS Error Status depends on
  the cause of the warning.

The ASIS-for-GNAT warning mode may be set when initializing the ASIS
implementation. The ``-ws`` parameter of
``Asis.Implementation.Initialize``
query suppresses warnings, the ``-we``
parameter of this query sets treating all the warnings as errors. When set,
the warning mode remains the same for all ``Context``\ s processed until
ASIS-for-GNAT has completed.

.. index:: Asis.Implementation.Initialize procedure
