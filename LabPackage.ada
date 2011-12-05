-- PRO-1 -----------------------------------------
-- Labwork-1 -------------------------------------
-- Created by Gula Vadym -------------------------
-- Group IO-92 -----------------------------------
-- 1.6  F1:  a = ( B * C ) -----------------------
-- 2.21 F2: MD = MA + ( MB * MC ) ----------------
-- 3.27 F3: E = SORT( MA * MA * C - D + W ) ------

with ada.text_iO;
package Int_IO is new Ada.text_IO.Integer_IO ( Integer ) ;
with Int_IO; with Ada.Text_IO;

package LabPackage is

   N : Integer := 3; -- Size of vector and matrix

   type Vector is private;
   type Matrix is private;
   
   procedure Func1(B : in Vector; C : in Vector; Asmall : out Integer);
   
   function Func2(MA : in Matrix; MB : in Matrix; MC : in Matrix) return Matrix;
   
   function Func3(MA : in Matrix; C : in Vector; D : in Vector; W : in Vector) return Vector;
   
   procedure Input_Matrix(MM : out Matrix);

   procedure Output_Matrix(MM : in Matrix);

   procedure Input_Vector(V : out Vector);

   procedure Output_Vector(V : in Vector);
   
   private 
      type Vector is array (1 .. N) of Integer;
      type Matrix is array (1 .. N, 1 .. N) of Integer;
end LabPackage;

package body LabPackage is

   procedure Sort_Vector(V_In : in Vector; V_Out : out Vector) is
      I, J   : Integer;
      Buffer : Integer;
      begin
         for I in 1..N loop
            Buffer := V_In(I);
            for J in I..N - 1 loop
               if ( V_In(J) > V_In(J + 1) ) then
                  Buffer := V_In(J + 1);
               end if;
            end loop;
            V_Out(I) := Buffer;
         end loop;
      end Sort_Vector;
      
   function Matrix_Sum(MA : in Matrix; MB : in Matrix) return Matrix is
   	MM   : Matrix;
	   I, J : Integer;
      begin
         for I in 1..N loop
            for J in 1..N loop
               MM(I, J) := MA(I, J) + MB(I, J);
            end loop;
         end loop;
         return MM;
      end Matrix_Sum;
   
   function Vector_Sub(A : in Vector; b : in Vector) return Vector is
   	V : Vector;
	   I : Integer;
      begin  	
         for I in 1..N loop
            V(I) := A(I) - B(I);	
         end loop;
         return V;
      end Vector_Sub;
      
   function Vector_Sum(A : in vector; B : in Vector) return Vector is
   	V : Vector;
      I : Integer;
      begin  	
         for I in 1..N loop
            V(I) := A(I) + B(I);	
         end loop;
         return V;
      end Vector_Sum;
   
   function Read_Integer_Number return Integer is
      Buffer : Integer;
      begin
         Int_IO.Get(Buffer);
         return Buffer;
      end Read_Integer_Number;
      
   function Matrix_Multiply( MA : in Matrix; MB : in Matrix)
   return Matrix is
      MM      : Matrix;
	   I, J, K : integer;
      begin    	
         for I in 1..N loop
            for J in 1..N loop
               MM(I, J) := 0;
               for K in 1..N loop
                  MM(I, J) := MM(I, J) + MA(I, K) * MB(K, J);
               end loop;
            end loop;
         end loop;
         return MM;
   end Matrix_Multiply;
   
   function Matrix_Vector_Multiply(MA : in Matrix; B : in Vector) 
   return vector is
      V       : vector;
	   I, J, K : integer;
   begin
	     for I in 1..N loop
		 	for J in 1..N loop
				V(I) := 0;
				for K in 1..N loop
				   V(I) := V(I) + MA(I, K) * B(K);
			    end loop;
		    end loop;
	     end loop;
		 return V;
    end matrix_vector_multiply;
      
   procedure Input_Matrix(MM : out Matrix) is
      I, J : integer;
      begin  
         for I in 1..N loop
            for J in 1..N loop
               Int_IO.Get( MM(I, J) );
            end loop;
         end loop;
      end Input_Matrix;

   procedure Output_Matrix(MM : in Matrix) is
   	  I, J : integer;
      begin  
         for I in 1..N loop
            for J in 1..N loop
               Int_IO.Put( MM(I, J) );
            end loop;
            Ada.text_IO.New_Line;
         end loop;
      end Output_Matrix;


   procedure Input_Vector(V : out Vector) is
   	I : Integer;
      begin  
         for I in 1..N loop 
            Int_IO.Get( V(I) );	
         end loop;	
      end Input_Vector;

   procedure Output_Vector(V : in Vector) is
   	I : Integer;
      begin   
  	     for I in 1..N loop 
		    Int_IO.Put( V(I) );	
	     end loop;
      end Output_Vector;
      
   procedure Func1(B : in Vector; C : in Vector; Asmall : out Integer) is
      I : integer;
      begin
         Asmall := 0;
         for I in 1..N loop
            Asmall := Asmall + B(I) * C(I);
         end loop;
      end Func1;
      
   function Func2(MA : in Matrix; MB : in Matrix; MC : in Matrix)
   return Matrix is
      MM : Matrix;
      begin
         MM := Matrix_Multiply(MB, MC);
         MM := Matrix_Sum(MM, MA);
         return MM;
      end Func2;
      
   function Func3(MA : in Matrix; C : in Vector; D : in Vector; W : in Vector)
   return Vector is
      V1 : Vector;
      V2 : Vector;
      MM : Matrix;
      begin
         MM := Matrix_Multiply(MA, MA);
         V1 := Matrix_Vector_Multiply(MM, C);
         V1 := Vector_Sub(V1, D);
         V1 := Vector_Sum(V1, W);
         Sort_Vector(V1, V2);
         return V2;
      end Func3;
   
begin

   Ada.Text_IO.Put_Line("Line");
   
end LabPackage;
