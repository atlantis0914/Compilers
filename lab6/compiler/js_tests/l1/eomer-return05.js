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
    var c0_var_t0 = 0;
    var c0_var_t1 = 0;
    var c0_var_t10 = 0;
    var c0_var_t11 = 0;
    var c0_var_t12 = 0;
    var c0_var_t13 = 0;
    var c0_var_t14 = 0;
    var c0_var_t15 = 0;
    var c0_var_t16 = 0;
    var c0_var_t17 = 0;
    var c0_var_t18 = 0;
    var c0_var_t19 = 0;
    var c0_var_t2 = 0;
    var c0_var_t20 = 0;
    var c0_var_t21 = 0;
    var c0_var_t22 = 0;
    var c0_var_t23 = 0;
    var c0_var_t24 = 0;
    var c0_var_t25 = 0;
    var c0_var_t26 = 0;
    var c0_var_t27 = 0;
    var c0_var_t28 = 0;
    var c0_var_t29 = 0;
    var c0_var_t3 = 0;
    var c0_var_t30 = 0;
    var c0_var_t31 = 0;
    var c0_var_t32 = 0;
    var c0_var_t33 = 0;
    var c0_var_t34 = 0;
    var c0_var_t35 = 0;
    var c0_var_t36 = 0;
    var c0_var_t37 = 0;
    var c0_var_t38 = 0;
    var c0_var_t39 = 0;
    var c0_var_t4 = 0;
    var c0_var_t40 = 0;
    var c0_var_t41 = 0;
    var c0_var_t42 = 0;
    var c0_var_t43 = 0;
    var c0_var_t44 = 0;
    var c0_var_t45 = 0;
    var c0_var_t46 = 0;
    var c0_var_t47 = 0;
    var c0_var_t48 = 0;
    var c0_var_t49 = 0;
    var c0_var_t5 = 0;
    var c0_var_t50 = 0;
    var c0_var_t6 = 0;
    var c0_var_t7 = 0;
    var c0_var_t8 = 0;
    var c0_var_t9 = 0;
     c0_var_t0  =  0 | 0;
     c0_var_t1  =  1 | 0;
     c0_var_t2  = ( 0  | 0) + ( 1  | 0)| 0;
     c0_var_t3  = ( 1  | 0) + ( c0_var_t2  | 0)| 0;
     c0_var_t4  = ( c0_var_t2  | 0) + ( c0_var_t3  | 0)| 0;
     c0_var_t5  = ( c0_var_t3  | 0) + ( c0_var_t4  | 0)| 0;
     c0_var_t6  = ( c0_var_t4  | 0) + ( c0_var_t5  | 0)| 0;
     c0_var_t7  = ( c0_var_t5  | 0) + ( c0_var_t6  | 0)| 0;
     c0_var_t8  = ( c0_var_t6  | 0) + ( c0_var_t7  | 0)| 0;
     c0_var_t9  = ( c0_var_t7  | 0) + ( c0_var_t8  | 0)| 0;
     c0_var_t10  = ( c0_var_t8  | 0) + ( c0_var_t9  | 0)| 0;
     c0_var_t11  = ( c0_var_t9  | 0) + ( c0_var_t10  | 0)| 0;
     c0_var_t12  = ( c0_var_t10  | 0) + ( c0_var_t11  | 0)| 0;
     c0_var_t13  = ( c0_var_t11  | 0) + ( c0_var_t12  | 0)| 0;
     c0_var_t14  = ( c0_var_t12  | 0) + ( c0_var_t13  | 0)| 0;
     c0_var_t15  = ( c0_var_t13  | 0) + ( c0_var_t14  | 0)| 0;
     c0_var_t16  = ( c0_var_t14  | 0) + ( c0_var_t15  | 0)| 0;
     c0_var_t17  = ( c0_var_t15  | 0) + ( c0_var_t16  | 0)| 0;
     c0_var_t18  = ( c0_var_t16  | 0) + ( c0_var_t17  | 0)| 0;
     c0_var_t19  = ( c0_var_t17  | 0) + ( c0_var_t18  | 0)| 0;
     c0_var_t20  = ( c0_var_t18  | 0) + ( c0_var_t19  | 0)| 0;
     c0_var_t21  = ( c0_var_t19  | 0) + ( c0_var_t20  | 0)| 0;
     c0_var_t22  = ( c0_var_t20  | 0) + ( c0_var_t21  | 0)| 0;
     c0_var_t23  = ( c0_var_t21  | 0) + ( c0_var_t22  | 0)| 0;
     c0_var_t24  = ( c0_var_t22  | 0) + ( c0_var_t23  | 0)| 0;
     c0_var_t25  = ( c0_var_t23  | 0) + ( c0_var_t24  | 0)| 0;
     c0_var_t26  = ( c0_var_t24  | 0) + ( c0_var_t25  | 0)| 0;
     c0_var_t27  = ( c0_var_t25  | 0) + ( c0_var_t26  | 0)| 0;
     c0_var_t28  = ( c0_var_t26  | 0) + ( c0_var_t27  | 0)| 0;
     c0_var_t29  = ( c0_var_t27  | 0) + ( c0_var_t28  | 0)| 0;
     c0_var_t30  = ( c0_var_t28  | 0) + ( c0_var_t29  | 0)| 0;
     c0_var_t31  = ( c0_var_t29  | 0) + ( c0_var_t30  | 0)| 0;
     c0_var_t32  = ( c0_var_t30  | 0) + ( c0_var_t31  | 0)| 0;
     c0_var_t33  = ( c0_var_t31  | 0) + ( c0_var_t32  | 0)| 0;
     c0_var_t34  = ( c0_var_t32  | 0) + ( c0_var_t33  | 0)| 0;
     c0_var_t35  = ( c0_var_t33  | 0) + ( c0_var_t34  | 0)| 0;
     c0_var_t36  = ( c0_var_t34  | 0) + ( c0_var_t35  | 0)| 0;
     c0_var_t37  = ( c0_var_t35  | 0) + ( c0_var_t36  | 0)| 0;
     c0_var_t38  = ( c0_var_t36  | 0) + ( c0_var_t37  | 0)| 0;
     c0_var_t39  = ( c0_var_t37  | 0) + ( c0_var_t38  | 0)| 0;
     c0_var_t40  = ( c0_var_t38  | 0) + ( c0_var_t39  | 0)| 0;
     c0_var_t41  = ( c0_var_t39  | 0) + ( c0_var_t40  | 0)| 0;
     c0_var_t42  = ( c0_var_t40  | 0) + ( c0_var_t41  | 0)| 0;
     c0_var_t43  = ( c0_var_t41  | 0) + ( c0_var_t42  | 0)| 0;
     c0_var_t44  = ( c0_var_t42  | 0) + ( c0_var_t43  | 0)| 0;
     c0_var_t45  = ( c0_var_t43  | 0) + ( c0_var_t44  | 0)| 0;
     c0_var_t46  = ( c0_var_t44  | 0) + ( c0_var_t45  | 0)| 0;
     c0_var_t47  = ( c0_var_t45  | 0) + ( c0_var_t46  | 0)| 0;
     c0_var_t48  = ( c0_var_t46  | 0) + ( c0_var_t47  | 0)| 0;
     c0_var_t49  = ( c0_var_t47  | 0) + ( c0_var_t48  | 0)| 0;
     c0_var_t50  = ( c0_var_t48  | 0) + ( c0_var_t49  | 0)| 0;
    return ((((((((((((((((((((((((((((((((((((((((((((((((((( 0  | 0) + ( 1  | 0) | 0) + ( c0_var_t2  | 0) | 0) + ( c0_var_t3  | 0) | 0) + ( c0_var_t4  | 0) | 0) + ( c0_var_t5  | 0) | 0) + ( c0_var_t6  | 0) | 0) + ( c0_var_t7  | 0) | 0) + ( c0_var_t8  | 0) | 0) + ( c0_var_t9  | 0) | 0) + ( c0_var_t10  | 0) | 0) + ( c0_var_t11  | 0) | 0) + ( c0_var_t12  | 0) | 0) + ( c0_var_t13  | 0) | 0) + ( c0_var_t14  | 0) | 0) + ( c0_var_t15  | 0) | 0) + ( c0_var_t16  | 0) | 0) + ( c0_var_t17  | 0) | 0) + ( c0_var_t18  | 0) | 0) + ( c0_var_t19  | 0) | 0) + ( c0_var_t20  | 0) | 0) + ( c0_var_t21  | 0) | 0) + ( c0_var_t22  | 0) | 0) + ( c0_var_t23  | 0) | 0) + ( c0_var_t24  | 0) | 0) + ( c0_var_t25  | 0) | 0) + ( c0_var_t26  | 0) | 0) + ( c0_var_t27  | 0) | 0) + ( c0_var_t28  | 0) | 0) + ( c0_var_t29  | 0) | 0) + ( c0_var_t30  | 0) | 0) + ( c0_var_t31  | 0) | 0) + ( c0_var_t32  | 0) | 0) + ( c0_var_t33  | 0) | 0) + ( c0_var_t34  | 0) | 0) + ( c0_var_t35  | 0) | 0) + ( c0_var_t36  | 0) | 0) + ( c0_var_t37  | 0) | 0) + ( c0_var_t38  | 0) | 0) + ( c0_var_t39  | 0) | 0) + ( c0_var_t40  | 0) | 0) + ( c0_var_t41  | 0) | 0) + ( c0_var_t42  | 0) | 0) + ( c0_var_t43  | 0) | 0) + ( c0_var_t44  | 0) | 0) + ( c0_var_t45  | 0) | 0) + ( c0_var_t46  | 0) | 0) + ( c0_var_t47  | 0) | 0) + ( c0_var_t48  | 0) | 0) + ( c0_var_t49  | 0) | 0) + ( c0_var_t50  | 0) | 0);

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