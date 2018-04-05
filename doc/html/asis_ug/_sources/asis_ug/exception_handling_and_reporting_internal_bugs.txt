.. _Exception_Handling_and_Reporting_Internal_Bugs:

**********************************************
Exception Handling and Reporting Internal Bugs
**********************************************

According to the ASIS Standard, only ASIS-defined exceptions can
be propagated from ASIS queries. The same holds for the
ASIS Extensions queries supported by ASIS-for-GNAT.

If a non-ASIS exception is raised during the processing of
an ASIS or ASIS extension query, this symptom reflects
an internal implementation problem. Under such a circumstance,
by default the ASIS query will output some diagnostic information
to ``Standard_Error`` and then exit to the OS; that is,
the execution of the ASIS application is aborted.

In order to allow the execution of an ASIS-based program
to continue even in case of such internal ASIS
implementation errors, you can change the default behavior by supplying
appropriate parameters to ``Asis.Implementation.Initialize``. See
ASIS-for-GNAT Reference Manual
for more details.
