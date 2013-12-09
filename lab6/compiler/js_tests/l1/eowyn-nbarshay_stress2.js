function fadd(x, y) {
  return x + y;
}

function fsub(x, y) {
  return x - y;
}

function fmul(x, y) {
  return x * y;
}

function fdiv(x, y) {
  return x / y;
}

function fless(x, y) {
 return (x < y);
}

function itof(x) {
  return x;
}

function ftoi(x) {
  return x;
}

function print_fpt(x) {
  return x;
}

function print_int(x) {
  return x;
}

function print_hex(x) {
  return x;
}

foreignImports = {
  fadd : fadd,
  fsub : fsub,
  fmul : fmul,
  fdiv : fdiv,
  fless : fless,
  itof : itof,
  ftoi : ftoi,
  print_fpt : print_fpt,
  print_int : print_int,
  print_hex : print_hex
}

function polyMul(a, b) {
  var ah  = (a >>> 16) & 0xffff;
  var al = a & 0xffff;
  var bh  = (b >>> 16) & 0xffff;
  var bl = b & 0xffff;
  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
}
if (typeof (this.Math.imul) == "undefined") {
  this.Math.imul = polyMul
}
function c0module(stdlib, foreign, heap) {
  "use asm";

  // Global Declarations
  var H32 = new stdlib.Int32Array(heap);
  var g_heapoff = 1;
  var g_stackoff = 0;

  // Global Exception Catchers
  var g_memex = 0;
  var g_oomex = 0;
  var g_numex = 0;
  var g_assex = 0;
  var imul = stdlib.Math.imul;
  var INTMIN = 0x80000000;

  // Function Declarations

  var fadd = foreign.fadd;
  var fsub = foreign.fsub;
  var fmul = foreign.fmul;
  var fdiv = foreign.fdiv;
  var fless = foreign.fless;
  var itof = foreign.itof;
  var ftoi = foreign.ftoi;
  var print_fpt = foreign.print_fpt;
  var print_int = foreign.print_int;
  var print_hex = foreign.print_hex;

  function memAlloc(size) {
    size = size | 0;
    var ret = 0
    if ((g_heapoff | 0) < (H32[0 | 0] | 0)) {
      ret = g_heapoff | 0;
      g_heapoff = (g_heapoff | 0) + (size | 0) | 0;
    } else {
      g_oomex = 1 | 0;
    }
   return ret | 0;
  }

  function memArrAlloc(size, numElems) {
    size = size | 0;
    numElems = numElems | 0;
    size = (size | 0) + (1 | 0) | 0;
    numElems = (size | 0) | 0;
    var ret = 0;
    if ((g_heapoff | 0) < (H32[0 | 0] | 0)) {
      ret = g_heapoff | 0;
      g_heapoff = (g_heapoff | 0) + (size | 0) | 0;
    } else {
      g_oomex = 1 | 0;
    }
    if (((numElems | 0) - (1 | 0) | 0) < (0 | 0)) {
      g_memex = 1 | 0;
    }
    H32[ret | 0] = ((numElems | 0) - (1 | 0)) | 0;
   return ret | 0;
  }

  function stackInit() {
    g_stackoff = (H32[0 | 0] | 0) - 1 | 0;
  }

  function polyDiv(num, den) {
    num = num | 0;
    den = den | 0;
    var ret = 0;
    ret = (num | 0) / (den | 0)| 0;
    if ((den | 0) == (0 | 0)) {
      g_numex = 1;
    }
    if (((num | 0) == (INTMIN | 0)) && ((den | 0) == (-1 | 0))) {
      g_numex = 1;
    }
    return ret | 0;
  }


  function polyMod(num, den) {
    num = num | 0;
    den = den | 0;
    var ret = 0;
    ret = (num | 0) % (den | 0)| 0;
    if ((den | 0) == (0 | 0)) {
      g_numex = 1;
    }
    if (((num | 0) == (INTMIN | 0)) && ((den | 0) == (-1 | 0))) {
      g_numex = 1;
    }
    return ret | 0;
  }


  function polyRShift(l, r) {
    l = l | 0;
    r = r | 0;
    var ret = 0;
    ret = (l | 0) >> (r | 0) | 0;
    if ((r | 0) >= (32 | 0)) {
      g_numex = 1;
    }
    if ((r | 0) < (0 | 0)) {
      g_numex = 1;
    }
    return ret | 0;
  }


  function polyLShift(l, r) {
    l = l | 0;
    r = r | 0;
    var ret = 0;
    ret = (l | 0) << (r | 0) | 0;
    if ((r | 0) >= (32 | 0)) {
      g_numex = 1;
    }
    if ((r | 0) < (0 | 0)) {
      g_numex = 1;
    }
    return ret | 0;
  }


  function pointerDeref(loc) {
    loc = loc | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return loc | 0;
  }

  function pointerLoad(loc) {
    loc = loc | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return H32[loc | 0] | 0;
  }

  function fieldAccess(loc, off) {
    loc = loc | 0;
    off = off | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return H32[loc + off | 0] | 0;
  }


  function arrAccess(loc, off) {
    loc = loc | 0;
    off = off | 0;
    if ((off | 0) < (0 | 0)) {
      g_memex = 1;
    }
    if ((off | 0) >= (H32[loc | 0] | 0)) {
      g_memex = 1 | 0;
    }
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return H32[loc + off + (1 | 0)] | 0;
  }


  function fieldShift(loc, off) {
    loc = loc | 0;
    off = off | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return (loc | 0) + (off | 0) | 0;
  }


  function arrShift(loc, off) {
    loc = loc | 0;
    off = off | 0;
    if ((off | 0) < (0 | 0)) {
      g_memex = 1;
    }
    if ((off | 0) >= (H32[loc | 0] | 0)) {
      g_memex = 1;
    }
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return (loc | 0) + (off | 0) + (1 | 0) | 0;
  }


  function memSet(loc, val) {
    loc = loc | 0;
    val = val | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    H32[loc | 0] = val | 0;
    return;
  }


  function assert(e) {
    e = e | 0
    if ((e | 0) != (1 | 0)) {
      g_assex = 1;
    }
    return;
  }


  function _c0_main () {
    var temp_ptr = 0;
    var c0_var_AB = 0;
    var c0_var_B = 0;
    var c0_var_C = 0;
    var c0_var_D = 0;
    var c0_var_E = 0;
    var c0_var_F = 0;
    var c0_var_G = 0;
    var c0_var_H = 0;
    var c0_var_I = 0;
    var c0_var_J = 0;
     c0_var_B  =  3402180 | 0;
     c0_var_B  = imul( c0_var_B  | 0, c0_var_B  | 0) | 0| 0;
     c0_var_C  = ( 9172  | 0) - ( c0_var_B  | 0)| 0;
     c0_var_C  = imul( c0_var_C  | 0, c0_var_B  | 0) | 0| 0;
     c0_var_C  = ( c0_var_C  | 0) + ( 4022  | 0)| 0;
     c0_var_D  =  5011 | 0;
     c0_var_E  =  4421 | 0;
     c0_var_F  =  5011 | 0;
     c0_var_B  = imul( c0_var_B  | 0, 6413  | 0) | 0| 0;
     c0_var_G  = ( c0_var_B  | 0) - ( c0_var_B  | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) - ( 6970  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) - (( c0_var_D  | 0) + ( 6808  | 0) | 0)| 0;
     c0_var_B  = imul( c0_var_B  | 0, c0_var_G  | 0) | 0| 0;
     c0_var_F  =  3368 | 0;
     c0_var_G  = ( c0_var_G  | 0) + (( 795  | 0) - (imul(imul( 97  | 0, 3368  | 0) | 0 | 0, c0_var_D  | 0) | 0 | 0) | 0)| 0;
     c0_var_C  = imul( c0_var_C  | 0, 8440  | 0) | 0| 0;
     c0_var_H  = (( c0_var_E  | 0) - ( c0_var_G  | 0) | 0) + ( 5306  | 0)| 0;
     c0_var_I  =  1528 | 0;
     c0_var_B  = ( c0_var_B  | 0) - ( 9503  | 0)| 0;
     c0_var_J  =  6340 | 0;
     c0_var_H  = ( c0_var_H  | 0) - ( c0_var_G  | 0)| 0;
     c0_var_AB  =  2379 | 0;
     c0_var_B  = ( c0_var_B  | 0) + ( c0_var_C  | 0)| 0;
     c0_var_E  = imul( c0_var_E  | 0,imul( 4914  | 0,( 1528  | 0) - ((( 6437  | 0) - ( 3368  | 0) | 0) - ( 6029  | 0) | 0) | 0) | 0 | 0) | 0| 0;
     c0_var_AB  =  6143 | 0;
     c0_var_J  = ( c0_var_J  | 0) - (imul( 5763  | 0, c0_var_B  | 0) | 0 | 0)| 0;
     c0_var_F  =  7917 | 0;
     c0_var_D  = imul( c0_var_D  | 0, c0_var_E  | 0) | 0| 0;
     c0_var_F  = ( c0_var_F  | 0) - ( 57555845  | 0)| 0;
     c0_var_C  =  c0_var_G | 0;
     c0_var_J  = ( c0_var_J  | 0) - ( 6143  | 0)| 0;
     c0_var_C  = ( c0_var_C  | 0) + ( c0_var_C  | 0)| 0;
     c0_var_H  =  c0_var_J | 0;
     c0_var_H  =  c0_var_D | 0;
     c0_var_F  = ( c0_var_F  | 0) - ( c0_var_F  | 0)| 0;
     c0_var_B  = imul( c0_var_B  | 0, c0_var_D  | 0) | 0| 0;
     c0_var_AB  = imul( c0_var_AB  | 0,( c0_var_C  | 0) + ( 5894  | 0) | 0) | 0| 0;
     c0_var_G  = ( c0_var_G  | 0) - ((( c0_var_E  | 0) - ((((( 1340  | 0) - ((imul(( 7445  | 0) + (( c0_var_D  | 0) - ( c0_var_AB  | 0) | 0) | 0, 7587  | 0) | 0 | 0) + ( c0_var_D  | 0) | 0) | 0) - ( c0_var_AB  | 0) | 0) + ((imul( 5667  | 0, c0_var_B  | 0) | 0 | 0) + ( c0_var_F  | 0) | 0) | 0) - ( c0_var_C  | 0) | 0) | 0) + ( c0_var_D  | 0) | 0)| 0;
     c0_var_B  = imul( c0_var_B  | 0, c0_var_J  | 0) | 0| 0;
     c0_var_G  =  c0_var_AB | 0;
     c0_var_I  = ( c0_var_I  | 0) + (( c0_var_AB  | 0) + (( c0_var_B  | 0) - ( c0_var_E  | 0) | 0) | 0)| 0;
     c0_var_F  = ( c0_var_F  | 0) - (( c0_var_F  | 0) + ( 8202  | 0) | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) - ( 9958  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) + ( c0_var_E  | 0)| 0;
     c0_var_AB  =  7567 | 0;
     c0_var_B  = ( c0_var_B  | 0) + ( c0_var_J  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) + ( 5363  | 0)| 0;
     c0_var_J  = imul( c0_var_J  | 0, 1152  | 0) | 0| 0;
     c0_var_C  = ( c0_var_C  | 0) - ( 5436  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) - ( c0_var_J  | 0)| 0;
     c0_var_E  = imul( c0_var_E  | 0, c0_var_F  | 0) | 0| 0;
     c0_var_C  =  428 | 0;
     c0_var_F  = imul( c0_var_F  | 0, 7567  | 0) | 0| 0;
     c0_var_AB  =  2215 | 0;
     c0_var_AB  = imul( c0_var_AB  | 0,imul( c0_var_AB  | 0, c0_var_B  | 0) | 0 | 0) | 0| 0;
     c0_var_E  = imul( c0_var_E  | 0, c0_var_J  | 0) | 0| 0;
     c0_var_C  =  8390 | 0;
     c0_var_B  = ( c0_var_B  | 0) - ( c0_var_D  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) + ( 8390  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) - ( 9201  | 0)| 0;
     c0_var_H  =  6312 | 0;
     c0_var_AB  = ( c0_var_AB  | 0) - ( 4916  | 0)| 0;
     c0_var_B  = imul( c0_var_B  | 0, c0_var_I  | 0) | 0| 0;
     c0_var_H  =  857 | 0;
     c0_var_AB  =  c0_var_D | 0;
     c0_var_C  = imul( c0_var_C  | 0, c0_var_AB  | 0) | 0| 0;
     c0_var_H  = ( c0_var_H  | 0) - ( c0_var_I  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) - ( 6856  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) + ( 7205  | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) + ( c0_var_E  | 0)| 0;
     c0_var_F  = ( c0_var_F  | 0) + ( 6607  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) - ( 1100  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) - ((( c0_var_I  | 0) - ( -7664  | 0) | 0) - ( c0_var_I  | 0) | 0)| 0;
     c0_var_C  = imul( c0_var_C  | 0, 5791  | 0) | 0| 0;
     c0_var_C  = imul( c0_var_C  | 0, 1491  | 0) | 0| 0;
     c0_var_J  = ( c0_var_J  | 0) - ( c0_var_B  | 0)| 0;
     c0_var_I  = imul( c0_var_I  | 0,imul((( c0_var_C  | 0) + ( 2338  | 0) | 0) - ( 7177  | 0) | 0, 3483  | 0) | 0 | 0) | 0| 0;
     c0_var_H  = ( c0_var_H  | 0) - ( 9911  | 0)| 0;
     c0_var_F  = ( c0_var_F  | 0) + ( c0_var_G  | 0)| 0;
     c0_var_G  = imul( c0_var_G  | 0, 9934  | 0) | 0| 0;
     c0_var_G  = ( c0_var_G  | 0) - ( c0_var_J  | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) - ( 6982  | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( c0_var_E  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) + ( c0_var_E  | 0)| 0;
     c0_var_F  =  2919 | 0;
     c0_var_E  = imul( c0_var_E  | 0, 235  | 0) | 0| 0;
     c0_var_B  = imul( c0_var_B  | 0, 6328  | 0) | 0| 0;
     c0_var_C  = imul( c0_var_C  | 0,imul( c0_var_E  | 0, c0_var_B  | 0) | 0 | 0) | 0| 0;
     c0_var_C  =  c0_var_AB | 0;
     c0_var_I  = ( c0_var_E  | 0) + ( 4639  | 0)| 0;
     c0_var_G  = ( c0_var_G  | 0) + ( c0_var_I  | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) - ((( c0_var_J  | 0) + (( c0_var_AB  | 0) + (( 4427  | 0) + ( c0_var_D  | 0) | 0) | 0) | 0) - ( c0_var_I  | 0) | 0)| 0;
     c0_var_D  =  c0_var_D | 0;
     c0_var_J  = imul( c0_var_J  | 0, 2919  | 0) | 0| 0;
     c0_var_H  = imul( c0_var_H  | 0,(( c0_var_D  | 0) + ( 9848  | 0) | 0) - (( 4154  | 0) + ( c0_var_G  | 0) | 0) | 0) | 0| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( 8701  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) + ( 6728  | 0)| 0;
     c0_var_H  = imul( c0_var_H  | 0, 2919  | 0) | 0| 0;
     c0_var_AB  = ( c0_var_AB  | 0) + ( 1052  | 0)| 0;
     c0_var_AB  = ( c0_var_AB  | 0) + ( 9016  | 0)| 0;
     c0_var_F  = ( c0_var_F  | 0) + ( c0_var_AB  | 0)| 0;
     c0_var_E  = imul( c0_var_E  | 0, 9426  | 0) | 0| 0;
     c0_var_B  = ( c0_var_B  | 0) - ( 1943  | 0)| 0;
     c0_var_C  = ( c0_var_C  | 0) + ( 7857  | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) + ( 2433  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) - ( 8825  | 0)| 0;
     c0_var_AB  = imul( c0_var_AB  | 0, 16191  | 0) | 0| 0;
     c0_var_I  = ( c0_var_I  | 0) - ( c0_var_E  | 0)| 0;
     c0_var_H  = ( c0_var_H  | 0) + ( 9932  | 0)| 0;
     c0_var_H  = ( c0_var_H  | 0) + ( c0_var_D  | 0)| 0;
     c0_var_H  = ( c0_var_H  | 0) - ( 5990  | 0)| 0;
     c0_var_AB  =  1892 | 0;
     c0_var_H  = imul( c0_var_H  | 0,imul((imul( 7643  | 0,( c0_var_B  | 0) - ( c0_var_G  | 0) | 0) | 0 | 0) - ( c0_var_J  | 0) | 0, c0_var_I  | 0) | 0 | 0) | 0| 0;
     c0_var_J  = ( c0_var_J  | 0) - ( c0_var_C  | 0)| 0;
     c0_var_C  = imul( c0_var_B  | 0, c0_var_H  | 0) | 0| 0;
     c0_var_D  = ( c0_var_D  | 0) - ( 1892  | 0)| 0;
     c0_var_F  = ( c0_var_F  | 0) - ( c0_var_G  | 0)| 0;
     c0_var_F  =  c0_var_G | 0;
     c0_var_G  =  8052 | 0;
     c0_var_C  = ( c0_var_C  | 0) - (( 6127  | 0) + ( c0_var_F  | 0) | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) + ( c0_var_I  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) - ( c0_var_H  | 0)| 0;
     c0_var_G  = ( c0_var_G  | 0) + ( c0_var_I  | 0)| 0;
     c0_var_C  = imul( c0_var_C  | 0, c0_var_F  | 0) | 0| 0;
     c0_var_C  = ( c0_var_C  | 0) - (( 1892  | 0) - ( 6987  | 0) | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) + ( c0_var_G  | 0)| 0;
     c0_var_B  = imul( c0_var_D  | 0,( c0_var_F  | 0) + (( c0_var_G  | 0) + ( 2538  | 0) | 0) | 0) | 0| 0;
     c0_var_G  = ( c0_var_G  | 0) - ( c0_var_B  | 0)| 0;
     c0_var_G  = imul( c0_var_G  | 0,imul(( 6039  | 0) - (imul(imul( 1790  | 0, c0_var_E  | 0) | 0 | 0, 4295  | 0) | 0 | 0) | 0, 3198  | 0) | 0 | 0) | 0| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( 8156  | 0)| 0;
     c0_var_B  = imul( c0_var_B  | 0,( c0_var_I  | 0) - ( 2761  | 0) | 0) | 0| 0;
     c0_var_F  = ( c0_var_F  | 0) - ( 1075  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) + ( 6289  | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( c0_var_E  | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) - (imul(imul( 3666  | 0, c0_var_D  | 0) | 0 | 0, 8131  | 0) | 0 | 0)| 0;
     c0_var_D  = imul( c0_var_D  | 0, 1183  | 0) | 0| 0;
     c0_var_I  = ( c0_var_I  | 0) + ( 5062  | 0)| 0;
     c0_var_F  = ( c0_var_F  | 0) - ( c0_var_C  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) + ( c0_var_G  | 0)| 0;
     c0_var_F  = ( c0_var_F  | 0) - ( c0_var_J  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) - ( 1641  | 0)| 0;
     c0_var_E  =  1892 | 0;
     c0_var_B  = ( c0_var_B  | 0) - ( c0_var_E  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) - ( 9613  | 0)| 0;
     c0_var_E  =  7829 | 0;
     c0_var_H  = ( c0_var_H  | 0) - ( 3024  | 0)| 0;
     c0_var_F  = imul( c0_var_F  | 0, 55175730  | 0) | 0| 0;
     c0_var_D  =  2351 | 0;
     c0_var_I  = ( c0_var_I  | 0) + (( 1892  | 0) - ( c0_var_C  | 0) | 0)| 0;
     c0_var_D  = imul( c0_var_D  | 0, 6773  | 0) | 0| 0;
     c0_var_E  = imul( c0_var_E  | 0, 3124  | 0) | 0| 0;
     c0_var_AB  = ( c0_var_AB  | 0) - ((( c0_var_G  | 0) - ( 4110607  | 0) | 0) - ( c0_var_D  | 0) | 0)| 0;
     c0_var_I  = imul( c0_var_I  | 0, 5960  | 0) | 0| 0;
     c0_var_D  = ( c0_var_D  | 0) + ( c0_var_H  | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) - (( c0_var_C  | 0) + ( c0_var_B  | 0) | 0)| 0;
     c0_var_H  = ( c0_var_H  | 0) + ( 2589  | 0)| 0;
     c0_var_D  = imul( c0_var_D  | 0, c0_var_H  | 0) | 0| 0;
     c0_var_H  =  c0_var_C | 0;
     c0_var_F  = ( c0_var_F  | 0) + ( c0_var_E  | 0)| 0;
     c0_var_G  = ( c0_var_G  | 0) + ( 3931  | 0)| 0;
     c0_var_D  = imul( c0_var_D  | 0, c0_var_AB  | 0) | 0| 0;
     c0_var_H  = ( c0_var_H  | 0) + ( c0_var_AB  | 0)| 0;
     c0_var_D  = imul( c0_var_D  | 0, 5340  | 0) | 0| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( 8762  | 0)| 0;
     c0_var_G  =  3370 | 0;
     c0_var_J  = imul( c0_var_J  | 0, 2639  | 0) | 0| 0;
     c0_var_J  = imul( c0_var_J  | 0, c0_var_D  | 0) | 0| 0;
     c0_var_J  = imul( c0_var_J  | 0, 4571  | 0) | 0| 0;
     c0_var_H  =  c0_var_B | 0;
     c0_var_G  =  c0_var_G | 0;
     c0_var_E  = imul( c0_var_E  | 0, c0_var_AB  | 0) | 0| 0;
     c0_var_F  = imul( c0_var_F  | 0, c0_var_D  | 0) | 0| 0;
     c0_var_E  = ( c0_var_E  | 0) - (( 82344840  | 0) - ( c0_var_H  | 0) | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) + ( 1598  | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( 2540  | 0)| 0;
     c0_var_AB  =  c0_var_AB | 0;
     c0_var_B  = imul( c0_var_B  | 0,imul( c0_var_E  | 0, 2251  | 0) | 0 | 0) | 0| 0;
     c0_var_F  = imul( c0_var_F  | 0, 19047726  | 0) | 0| 0;
     c0_var_J  = ( c0_var_J  | 0) + ( c0_var_AB  | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( 9819  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) - ( 7639  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) - ( c0_var_B  | 0)| 0;
     c0_var_G  = ( c0_var_G  | 0) - ( 28836690  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) + ( c0_var_E  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) + (( c0_var_H  | 0) + ( 1900  | 0) | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) + (( c0_var_I  | 0) - (( c0_var_H  | 0) + ( c0_var_F  | 0) | 0) | 0)| 0;
     c0_var_G  = ( c0_var_G  | 0) - ( c0_var_B  | 0)| 0;
     c0_var_G  = imul( c0_var_G  | 0,imul(imul(imul( c0_var_AB  | 0, c0_var_H  | 0) | 0 | 0, 2765  | 0) | 0 | 0,( 6616  | 0) - ( c0_var_AB  | 0) | 0) | 0 | 0) | 0| 0;
     c0_var_H  = ( c0_var_H  | 0) + ( 7776  | 0)| 0;
     c0_var_B  = imul( c0_var_B  | 0, 3786  | 0) | 0| 0;
     c0_var_G  = ( c0_var_G  | 0) - ( 2573000  | 0)| 0;
     c0_var_E  =  c0_var_F | 0;
     c0_var_D  = ( 17266  | 0) - ( c0_var_F  | 0)| 0;
     c0_var_I  = imul( c0_var_I  | 0, c0_var_H  | 0) | 0| 0;
     c0_var_J  =  c0_var_D | 0;
     c0_var_J  =  c0_var_C | 0;
     c0_var_H  = ( c0_var_H  | 0) - ( 3162  | 0)| 0;
     c0_var_G  = ((( c0_var_I  | 0) + ( c0_var_B  | 0) | 0) - ( 2880  | 0) | 0) - ( 2854  | 0)| 0;
     c0_var_C  = ( c0_var_C  | 0) + ( c0_var_C  | 0)| 0;
     c0_var_AB  = imul( c0_var_AB  | 0, 8681  | 0) | 0| 0;
     c0_var_H  = ( c0_var_H  | 0) + ( c0_var_F  | 0)| 0;
     c0_var_E  = imul( c0_var_E  | 0, c0_var_C  | 0) | 0| 0;
     c0_var_C  = imul( c0_var_C  | 0, c0_var_H  | 0) | 0| 0;
     c0_var_H  = ( c0_var_H  | 0) - ( 8371  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) + ( 9764  | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) - ( 335  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) - ( c0_var_AB  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) - ( c0_var_AB  | 0)| 0;
     c0_var_G  = ( c0_var_G  | 0) - ( 2255  | 0)| 0;
     c0_var_I  =  c0_var_B | 0;
     c0_var_H  = ( c0_var_H  | 0) + ( 2983  | 0)| 0;
     c0_var_E  = imul( c0_var_J  | 0, c0_var_I  | 0) | 0| 0;
     c0_var_E  = imul( c0_var_E  | 0, 7883  | 0) | 0| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( 4508736  | 0)| 0;
     c0_var_I  = imul( c0_var_I  | 0,( 4673  | 0) + (( c0_var_H  | 0) - ( 8012  | 0) | 0) | 0) | 0| 0;
     c0_var_D  = ( c0_var_D  | 0) + ( 3134  | 0)| 0;
     c0_var_J  = ( c0_var_J  | 0) + ( c0_var_J  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) + ( 1484  | 0)| 0;
     c0_var_B  =  c0_var_E | 0;
     c0_var_I  =  6952 | 0;
     c0_var_J  = ( c0_var_J  | 0) + ( 2525  | 0)| 0;
     c0_var_E  = ( c0_var_E  | 0) - ( 9560  | 0)| 0;
     c0_var_AB  =  c0_var_J | 0;
     c0_var_E  =  1832 | 0;
     c0_var_C  =  6952 | 0;
     c0_var_J  = ( c0_var_J  | 0) - ( 8341  | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) + ( 4901  | 0)| 0;
     c0_var_G  = imul( c0_var_G  | 0, 732  | 0) | 0| 0;
     c0_var_C  =  5457 | 0;
     c0_var_J  = ( c0_var_J  | 0) + ( 6169  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) + ( 5018  | 0)| 0;
     c0_var_E  = imul( c0_var_E  | 0, c0_var_F  | 0) | 0| 0;
     c0_var_B  =  5457 | 0;
     c0_var_F  = ( c0_var_F  | 0) - (imul( c0_var_I  | 0,imul(( 6630  | 0) + ( c0_var_B  | 0) | 0, 1492  | 0) | 0 | 0) | 0 | 0)| 0;
     c0_var_F  =  c0_var_F | 0;
     c0_var_G  = imul( c0_var_G  | 0, c0_var_F  | 0) | 0| 0;
     c0_var_H  =  8602 | 0;
     c0_var_D  = imul( c0_var_D  | 0, c0_var_B  | 0) | 0| 0;
     c0_var_J  =  7654 | 0;
     c0_var_I  = ( c0_var_I  | 0) + ( 6088  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) - ( 3694  | 0)| 0;
     c0_var_I  = imul( c0_var_I  | 0, c0_var_AB  | 0) | 0| 0;
     c0_var_B  = ( c0_var_B  | 0) - ( c0_var_AB  | 0)| 0;
     c0_var_AB  = imul( c0_var_AB  | 0,imul( c0_var_D  | 0, 4641  | 0) | 0 | 0) | 0| 0;
     c0_var_I  = imul( c0_var_I  | 0, 2790  | 0) | 0| 0;
     c0_var_I  = ( c0_var_I  | 0) - ( 5457  | 0)| 0;
     c0_var_I  = imul(( c0_var_F  | 0) - ( 8602  | 0) | 0, c0_var_F  | 0) | 0| 0;
     c0_var_AB  =  c0_var_I | 0;
     c0_var_I  =  2986 | 0;
     c0_var_I  = ( c0_var_I  | 0) + ( 8602  | 0)| 0;
    return ( c0_var_AB  | 0);

  }

  function main() {
    stackInit();
    return _c0_main() | 0;
  }

  function getNumEx() {
    return g_numex | 0;
  }
  function getMemEx() {
    return g_memex | 0;
  }
  function getOomEx() {
    return g_oomex | 0;
  }
  function getAssEx() {
    return g_assex | 0;
  }
  return { main : main , getNumEx : getNumEx, getMemEx : getMemEx, getOomEx : getOomEx, getAssEx : getAssEx}

}
var c0arr = new Int32Array(5000000)
c0arr[0] = 5000000;
var c0_export = c0module(this, foreignImports, c0arr);
var res = (c0_export.main())
var numEx = (c0_export.getNumEx())
var memEx = (c0_export.getMemEx())
var assEx = (c0_export.getAssEx())
print("Result: " + res)
print("NumEx: " + numEx)
print("MemEx: " + memEx)
print("AssEx: " + assEx)