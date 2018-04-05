
package Root_Package.Child_Package is
   --  The above reference to Root_Package is a cross-file reference.

   use Child_Package;
   --  The above reference to Child_Package is an intra-file reference.

   use root_package.child_package;

end Root_Package.Child_Package;
