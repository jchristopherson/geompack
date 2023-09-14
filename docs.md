---
project: GEOMPACK
summary: A modernization of the GEOMPACK library for computing Delaunay triangulations.
project_github: https://github.com/jchristopherson/geompack
author: Original F77 by Barry Joe, F90 update by John Burkardt, Modernization by Jason Christopherson
author_email: jchristopherson@hotmail.com
src_dir: ./src
output_dir: ./doc
display: public
         protected
source: true
proc_internals: true
sort: permission-alpha
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            tomlf:https://toml-f.github.io/toml-f
            M_CLI2:https://github.com/urbanjost/M_CLI2
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---