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
    var c0_var_i1 = 0;
    var c0_var_i10 = 0;
    var c0_var_i11 = 0;
    var c0_var_i12 = 0;
    var c0_var_i13 = 0;
    var c0_var_i14 = 0;
    var c0_var_i15 = 0;
    var c0_var_i2 = 0;
    var c0_var_i3 = 0;
    var c0_var_i4 = 0;
    var c0_var_i5 = 0;
    var c0_var_i6 = 0;
    var c0_var_i7 = 0;
    var c0_var_i8 = 0;
    var c0_var_i9 = 0;
     c0_var_i1  =  3 | 0;
     c0_var_i2  =  -30 | 0;
     c0_var_i3  =  -254 | 0;
     c0_var_i4  = ( 1  | 0) - (polyDiv( 15  | 0, 17  | 0) | 0 | 0)| 0;
     c0_var_i5  =  9 | 0;
     c0_var_i6  = ( 1  | 0) + (polyDiv( 2  | 0, 2  | 0) | 0 | 0)| 0;
     c0_var_i7  =  33 | 0;
     c0_var_i8  =  31 | 0;
     c0_var_i9  = polyDiv( 32  | 0, 2  | 0) | 0| 0;
     c0_var_i10  = (polyDiv( 72  | 0, 4  | 0) | 0 | 0) + ( 12  | 0)| 0;
     c0_var_i11  = (polyDiv( 72  | 0, 4  | 0) | 0 | 0) - ( 12  | 0)| 0;
     c0_var_i12  = imul(polyDiv( 72  | 0, 4  | 0) | 0 | 0, 12  | 0) | 0| 0;
     c0_var_i13  = polyDiv(polyDiv( 72  | 0, 4  | 0) | 0 | 0, 2  | 0) | 0| 0;
     c0_var_i14  =  0 | 0;
     c0_var_i14  = ( c0_var_i14  | 0) + (( 3  | 0) - ( -30  | 0) | 0)| 0;
     c0_var_i14  = ( c0_var_i14  | 0) - (( -254  | 0) + ( c0_var_i4  | 0) | 0)| 0;
     c0_var_i14  = imul( c0_var_i14  | 0,imul( 9  | 0, c0_var_i6  | 0) | 0 | 0) | 0| 0;
     c0_var_i14  = polyDiv( c0_var_i14  | 0,polyDiv( 33  | 0, 31  | 0) | 0 | 0) | 0| 0;
     c0_var_i15  = (((((( 44  | 0) - (polyDiv(polyDiv( 1462  | 0, 12  | 0) | 0 | 0, 12  | 0) | 0 | 0) | 0) - ( 333  | 0) | 0) + ( 165  | 0) | 0) - (polyMod( 7  | 0, 4  | 0) | 0 | 0) | 0) - (polyMod(imul(polyDiv( 8  | 0, 15  | 0) | 0 | 0, 5  | 0) | 0 | 0, 257  | 0) | 0 | 0) | 0) - ( 13  | 0)| 0;
    return ((((((( 3  | 0) + ( -30  | 0) | 0) - (polyMod(polyDiv(imul( -254  | 0, c0_var_i4  | 0) | 0 | 0, 15  | 0) | 0 | 0, c0_var_i6  | 0) | 0 | 0) | 0) + ( 33  | 0) | 0) - (polyMod(polyDiv(imul( 31  | 0, c0_var_i9  | 0) | 0 | 0, c0_var_i10  | 0) | 0 | 0, c0_var_i13  | 0) | 0 | 0) | 0) + ( c0_var_i14  | 0) | 0) - ( c0_var_i15  | 0) | 0);

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