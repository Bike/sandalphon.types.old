;;;; figures.lisp

(in-package #:sandalphon.types-test)

(defparameter *standard-atomic-type-specifiers*
'(
arithmetic-error                  function            simple-condition           
array                             generic-function    simple-error               
atom                              hash-table          simple-string              
base-char                         integer             simple-type-error          
base-string                       keyword             simple-vector              
bignum                            list                simple-warning             
bit                               logical-pathname    single-float               
bit-vector                        long-float          standard-char              
broadcast-stream                  method              standard-class             
built-in-class                    method-combination  standard-generic-function  
cell-error                        nil                 standard-method            
character                         null                standard-object            
class                             number              storage-condition          
compiled-function                 package             stream                     
complex                           package-error       stream-error               
concatenated-stream               parse-error         string                     
condition                         pathname            string-stream              
cons                              print-not-readable  structure-class            
control-error                     program-error       structure-object           
division-by-zero                  random-state        style-warning              
double-float                      ratio               symbol                     
echo-stream                       rational            synonym-stream             
end-of-file                       reader-error        t                          
error                             readtable           two-way-stream             
extended-char                     real                type-error                 
file-error                        restart             unbound-slot               
file-stream                       sequence            unbound-variable           
fixnum                            serious-condition   undefined-function         
float                             short-float         unsigned-byte              
floating-point-inexact            signed-byte         vector                     
floating-point-invalid-operation  simple-array        warning                    
floating-point-overflow           simple-base-string                             
floating-point-underflow          simple-bit-vector
) "CLHS figure 4-2, from 4.2.3 Type Specifiers. Standard atomic type specifiers.")

(defparameter *standard-compound-type-specifiers*
'(
and           long-float    simple-base-string  
array         member        simple-bit-vector   
base-string   mod           simple-string       
bit-vector    not           simple-vector       
complex       or            single-float        
cons          rational      string              
double-float  real          unsigned-byte       
eql           satisfies     values              
float         short-float   vector              
function      signed-byte                       
integer       simple-array
) "CLHS figure 4-3, from 4.2.3 Type Specifiers. Standard type specifiers that have compound forms.")

(defparameter *standard-compound-only-type-specifiers*
'(
and     mod  satisfies  
eql     not  values     
member  or
) "CLHS figure 4-4, from 4.2.3 Type Specifiers. Type specifiers with a compound form but not an atomic form.
Note that programmers cannot define their own.")

(defparameter *types-with-classes*
'(
arithmetic-error                  generic-function    simple-error               
array                             hash-table          simple-type-error          
bit-vector                        integer             simple-warning             
broadcast-stream                  list                standard-class             
built-in-class                    logical-pathname    standard-generic-function  
cell-error                        method              standard-method            
character                         method-combination  standard-object            
class                             null                storage-condition          
complex                           number              stream                     
concatenated-stream               package             stream-error               
condition                         package-error       string                     
cons                              parse-error         string-stream              
control-error                     pathname            structure-class            
division-by-zero                  print-not-readable  structure-object           
echo-stream                       program-error       style-warning              
end-of-file                       random-state        symbol                     
error                             ratio               synonym-stream             
file-error                        rational            t                          
file-stream                       reader-error        two-way-stream             
float                             readtable           type-error                 
floating-point-inexact            real                unbound-slot               
floating-point-invalid-operation  restart             unbound-variable           
floating-point-overflow           sequence            undefined-function         
floating-point-underflow          serious-condition   vector                     
function                          simple-condition    warning
) "Figure 4-8. Classes that correspond to pre-defined type specifiers (from CLHS 4.3.7)")
