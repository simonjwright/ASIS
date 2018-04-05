package Root_Package is
   package Public_Package is
   end Public_Package;
   use Public_Package;
private
   package Private_Package is
   end Private_Package;
   use Private_Package;
end Root_Package;
