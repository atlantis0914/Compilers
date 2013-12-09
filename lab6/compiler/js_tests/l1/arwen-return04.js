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
    var c0_var_first = 0;
    var c0_var_fourth = 0;
    var c0_var_result = 0;
    var c0_var_second = 0;
    var c0_var_third = 0;
    var c0_var_v0 = 0;
    var c0_var_v1 = 0;
    var c0_var_v10 = 0;
    var c0_var_v100 = 0;
    var c0_var_v101 = 0;
    var c0_var_v102 = 0;
    var c0_var_v103 = 0;
    var c0_var_v104 = 0;
    var c0_var_v105 = 0;
    var c0_var_v106 = 0;
    var c0_var_v107 = 0;
    var c0_var_v108 = 0;
    var c0_var_v109 = 0;
    var c0_var_v11 = 0;
    var c0_var_v110 = 0;
    var c0_var_v111 = 0;
    var c0_var_v112 = 0;
    var c0_var_v113 = 0;
    var c0_var_v114 = 0;
    var c0_var_v115 = 0;
    var c0_var_v116 = 0;
    var c0_var_v117 = 0;
    var c0_var_v118 = 0;
    var c0_var_v119 = 0;
    var c0_var_v12 = 0;
    var c0_var_v120 = 0;
    var c0_var_v121 = 0;
    var c0_var_v122 = 0;
    var c0_var_v123 = 0;
    var c0_var_v124 = 0;
    var c0_var_v125 = 0;
    var c0_var_v126 = 0;
    var c0_var_v127 = 0;
    var c0_var_v13 = 0;
    var c0_var_v14 = 0;
    var c0_var_v15 = 0;
    var c0_var_v16 = 0;
    var c0_var_v17 = 0;
    var c0_var_v18 = 0;
    var c0_var_v19 = 0;
    var c0_var_v2 = 0;
    var c0_var_v20 = 0;
    var c0_var_v21 = 0;
    var c0_var_v22 = 0;
    var c0_var_v23 = 0;
    var c0_var_v24 = 0;
    var c0_var_v25 = 0;
    var c0_var_v26 = 0;
    var c0_var_v27 = 0;
    var c0_var_v28 = 0;
    var c0_var_v29 = 0;
    var c0_var_v3 = 0;
    var c0_var_v30 = 0;
    var c0_var_v31 = 0;
    var c0_var_v32 = 0;
    var c0_var_v33 = 0;
    var c0_var_v34 = 0;
    var c0_var_v35 = 0;
    var c0_var_v36 = 0;
    var c0_var_v37 = 0;
    var c0_var_v38 = 0;
    var c0_var_v39 = 0;
    var c0_var_v4 = 0;
    var c0_var_v40 = 0;
    var c0_var_v41 = 0;
    var c0_var_v42 = 0;
    var c0_var_v43 = 0;
    var c0_var_v44 = 0;
    var c0_var_v45 = 0;
    var c0_var_v46 = 0;
    var c0_var_v47 = 0;
    var c0_var_v48 = 0;
    var c0_var_v49 = 0;
    var c0_var_v5 = 0;
    var c0_var_v50 = 0;
    var c0_var_v51 = 0;
    var c0_var_v52 = 0;
    var c0_var_v53 = 0;
    var c0_var_v54 = 0;
    var c0_var_v55 = 0;
    var c0_var_v56 = 0;
    var c0_var_v57 = 0;
    var c0_var_v58 = 0;
    var c0_var_v59 = 0;
    var c0_var_v6 = 0;
    var c0_var_v60 = 0;
    var c0_var_v61 = 0;
    var c0_var_v62 = 0;
    var c0_var_v63 = 0;
    var c0_var_v64 = 0;
    var c0_var_v65 = 0;
    var c0_var_v66 = 0;
    var c0_var_v67 = 0;
    var c0_var_v68 = 0;
    var c0_var_v69 = 0;
    var c0_var_v7 = 0;
    var c0_var_v70 = 0;
    var c0_var_v71 = 0;
    var c0_var_v72 = 0;
    var c0_var_v73 = 0;
    var c0_var_v74 = 0;
    var c0_var_v75 = 0;
    var c0_var_v76 = 0;
    var c0_var_v77 = 0;
    var c0_var_v78 = 0;
    var c0_var_v79 = 0;
    var c0_var_v8 = 0;
    var c0_var_v80 = 0;
    var c0_var_v81 = 0;
    var c0_var_v82 = 0;
    var c0_var_v83 = 0;
    var c0_var_v84 = 0;
    var c0_var_v85 = 0;
    var c0_var_v86 = 0;
    var c0_var_v87 = 0;
    var c0_var_v88 = 0;
    var c0_var_v89 = 0;
    var c0_var_v9 = 0;
    var c0_var_v90 = 0;
    var c0_var_v91 = 0;
    var c0_var_v92 = 0;
    var c0_var_v93 = 0;
    var c0_var_v94 = 0;
    var c0_var_v95 = 0;
    var c0_var_v96 = 0;
    var c0_var_v97 = 0;
    var c0_var_v98 = 0;
    var c0_var_v99 = 0;
     c0_var_v0  =  0 | 0;
     c0_var_v1  =  1 | 0;
     c0_var_v2  =  2 | 0;
     c0_var_v3  =  3 | 0;
     c0_var_v4  =  4 | 0;
     c0_var_v5  =  5 | 0;
     c0_var_v6  =  6 | 0;
     c0_var_v7  =  7 | 0;
     c0_var_v8  =  8 | 0;
     c0_var_v9  =  9 | 0;
     c0_var_v10  =  10 | 0;
     c0_var_v11  =  11 | 0;
     c0_var_v12  =  12 | 0;
     c0_var_v13  =  13 | 0;
     c0_var_v14  =  14 | 0;
     c0_var_v15  =  15 | 0;
     c0_var_v16  =  16 | 0;
     c0_var_v17  =  17 | 0;
     c0_var_v18  =  18 | 0;
     c0_var_v19  =  19 | 0;
     c0_var_v20  =  20 | 0;
     c0_var_v21  =  21 | 0;
     c0_var_v22  =  22 | 0;
     c0_var_v23  =  23 | 0;
     c0_var_v24  =  24 | 0;
     c0_var_v25  =  25 | 0;
     c0_var_v26  =  26 | 0;
     c0_var_v27  =  27 | 0;
     c0_var_v28  =  28 | 0;
     c0_var_v29  =  29 | 0;
     c0_var_v30  =  30 | 0;
     c0_var_v31  =  31 | 0;
     c0_var_v32  =  32 | 0;
     c0_var_v33  =  33 | 0;
     c0_var_v34  =  34 | 0;
     c0_var_v35  =  35 | 0;
     c0_var_v36  =  36 | 0;
     c0_var_v37  =  37 | 0;
     c0_var_v38  =  38 | 0;
     c0_var_v39  =  39 | 0;
     c0_var_v40  =  40 | 0;
     c0_var_v41  =  41 | 0;
     c0_var_v42  =  42 | 0;
     c0_var_v43  =  43 | 0;
     c0_var_v44  =  44 | 0;
     c0_var_v45  =  45 | 0;
     c0_var_v46  =  46 | 0;
     c0_var_v47  =  47 | 0;
     c0_var_v48  =  48 | 0;
     c0_var_v49  =  49 | 0;
     c0_var_v50  =  50 | 0;
     c0_var_v51  =  51 | 0;
     c0_var_v52  =  52 | 0;
     c0_var_v53  =  53 | 0;
     c0_var_v54  =  54 | 0;
     c0_var_v55  =  55 | 0;
     c0_var_v56  =  56 | 0;
     c0_var_v57  =  57 | 0;
     c0_var_v58  =  58 | 0;
     c0_var_v59  =  59 | 0;
     c0_var_v60  =  60 | 0;
     c0_var_v61  =  61 | 0;
     c0_var_v62  =  62 | 0;
     c0_var_v63  =  63 | 0;
     c0_var_v64  =  64 | 0;
     c0_var_v65  =  65 | 0;
     c0_var_v66  =  66 | 0;
     c0_var_v67  =  67 | 0;
     c0_var_v68  =  68 | 0;
     c0_var_v69  =  69 | 0;
     c0_var_v70  =  70 | 0;
     c0_var_v71  =  71 | 0;
     c0_var_v72  =  72 | 0;
     c0_var_v73  =  73 | 0;
     c0_var_v74  =  74 | 0;
     c0_var_v75  =  75 | 0;
     c0_var_v76  =  76 | 0;
     c0_var_v77  =  77 | 0;
     c0_var_v78  =  78 | 0;
     c0_var_v79  =  79 | 0;
     c0_var_v80  =  80 | 0;
     c0_var_v81  =  81 | 0;
     c0_var_v82  =  82 | 0;
     c0_var_v83  =  83 | 0;
     c0_var_v84  =  84 | 0;
     c0_var_v85  =  85 | 0;
     c0_var_v86  =  86 | 0;
     c0_var_v87  =  87 | 0;
     c0_var_v88  =  88 | 0;
     c0_var_v89  =  89 | 0;
     c0_var_v90  =  90 | 0;
     c0_var_v91  =  91 | 0;
     c0_var_v92  =  92 | 0;
     c0_var_v93  =  93 | 0;
     c0_var_v94  =  94 | 0;
     c0_var_v95  =  95 | 0;
     c0_var_v96  =  96 | 0;
     c0_var_v97  =  97 | 0;
     c0_var_v98  =  98 | 0;
     c0_var_v99  =  99 | 0;
     c0_var_v100  =  100 | 0;
     c0_var_v101  =  101 | 0;
     c0_var_v102  =  102 | 0;
     c0_var_v103  =  103 | 0;
     c0_var_v104  =  104 | 0;
     c0_var_v105  =  105 | 0;
     c0_var_v106  =  106 | 0;
     c0_var_v107  =  107 | 0;
     c0_var_v108  =  108 | 0;
     c0_var_v109  =  109 | 0;
     c0_var_v110  =  110 | 0;
     c0_var_v111  =  111 | 0;
     c0_var_v112  =  112 | 0;
     c0_var_v113  =  113 | 0;
     c0_var_v114  =  114 | 0;
     c0_var_v115  =  115 | 0;
     c0_var_v116  =  116 | 0;
     c0_var_v117  =  117 | 0;
     c0_var_v118  =  118 | 0;
     c0_var_v119  =  119 | 0;
     c0_var_v120  =  120 | 0;
     c0_var_v121  =  121 | 0;
     c0_var_v122  =  122 | 0;
     c0_var_v123  =  123 | 0;
     c0_var_v124  =  124 | 0;
     c0_var_v125  =  125 | 0;
     c0_var_v126  =  126 | 0;
     c0_var_v127  =  127 | 0;
     c0_var_first  = (((((((( 1  | 0) + ( 9  | 0) | 0) + ( 17  | 0) | 0) + ( 25  | 0) | 0) + ( 33  | 0) | 0) + ( 41  | 0) | 0) + ( 49  | 0) | 0) + ( 57  | 0) | 0) + ( 65  | 0)| 0;
     c0_var_second  = (((((((( 2  | 0) + ( 10  | 0) | 0) + ( 18  | 0) | 0) + ( 26  | 0) | 0) + ( 34  | 0) | 0) + ( 42  | 0) | 0) + ( 50  | 0) | 0) + ( 58  | 0) | 0) + ( 66  | 0)| 0;
     c0_var_third  = (((((((( 3  | 0) + ( 11  | 0) | 0) + ( 19  | 0) | 0) + ( 27  | 0) | 0) + ( 35  | 0) | 0) + ( 43  | 0) | 0) + ( 51  | 0) | 0) + ( 59  | 0) | 0) + ( 67  | 0)| 0;
     c0_var_fourth  = (((((((( 4  | 0) + ( 12  | 0) | 0) + ( 20  | 0) | 0) + ( 28  | 0) | 0) + ( 36  | 0) | 0) + ( 44  | 0) | 0) + ( 52  | 0) | 0) + ( 60  | 0) | 0) + ( 68  | 0)| 0;
     c0_var_result  = (((((((( c0_var_first  | 0) + ( c0_var_second  | 0) | 0) + ( c0_var_third  | 0) | 0) + ( c0_var_fourth  | 0) | 0) + ( 7  | 0) | 0) + ( 8  | 0) | 0) + ( 9  | 0) | 0) + ( 10  | 0) | 0) + ( 11  | 0)| 0;
    return (( 64  | 0) + (imul( c0_var_result  | 0, 0  | 0) | 0 | 0) | 0);

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