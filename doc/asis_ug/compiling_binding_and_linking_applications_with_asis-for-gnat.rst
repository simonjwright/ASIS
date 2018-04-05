.. _Compiling_Binding_and_Linking_Applications_with_ASIS-for-GNAT:

***************************************************************
Compiling, Binding, and Linking Applications with ASIS-for-GNAT
***************************************************************

.. index:: ASIS-for-GNAT

The recommended way of building ASIS applications is to define for an
application a project file that depends on the main ASIS project file
``asis.gpr``. All you have to do is to add a with clause


.. code-block:: ada

  with "asis";


to the application project file. After that you can build an executable
for an application using *gprbuild* in the usual way.

.. index:: -lasis option
