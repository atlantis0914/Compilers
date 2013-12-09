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
    var c0_var_poop = 0;
    var c0_var_x_0 = 0;
    var c0_var_x_1 = 0;
    var c0_var_x_10 = 0;
    var c0_var_x_11 = 0;
    var c0_var_x_12 = 0;
    var c0_var_x_13 = 0;
    var c0_var_x_14 = 0;
    var c0_var_x_15 = 0;
    var c0_var_x_16 = 0;
    var c0_var_x_17 = 0;
    var c0_var_x_18 = 0;
    var c0_var_x_19 = 0;
    var c0_var_x_2 = 0;
    var c0_var_x_20 = 0;
    var c0_var_x_21 = 0;
    var c0_var_x_22 = 0;
    var c0_var_x_23 = 0;
    var c0_var_x_24 = 0;
    var c0_var_x_25 = 0;
    var c0_var_x_26 = 0;
    var c0_var_x_27 = 0;
    var c0_var_x_28 = 0;
    var c0_var_x_29 = 0;
    var c0_var_x_3 = 0;
    var c0_var_x_30 = 0;
    var c0_var_x_31 = 0;
    var c0_var_x_32 = 0;
    var c0_var_x_33 = 0;
    var c0_var_x_34 = 0;
    var c0_var_x_35 = 0;
    var c0_var_x_36 = 0;
    var c0_var_x_37 = 0;
    var c0_var_x_38 = 0;
    var c0_var_x_39 = 0;
    var c0_var_x_4 = 0;
    var c0_var_x_40 = 0;
    var c0_var_x_41 = 0;
    var c0_var_x_42 = 0;
    var c0_var_x_43 = 0;
    var c0_var_x_44 = 0;
    var c0_var_x_45 = 0;
    var c0_var_x_46 = 0;
    var c0_var_x_47 = 0;
    var c0_var_x_48 = 0;
    var c0_var_x_49 = 0;
    var c0_var_x_5 = 0;
    var c0_var_x_50 = 0;
    var c0_var_x_51 = 0;
    var c0_var_x_52 = 0;
    var c0_var_x_53 = 0;
    var c0_var_x_54 = 0;
    var c0_var_x_55 = 0;
    var c0_var_x_56 = 0;
    var c0_var_x_57 = 0;
    var c0_var_x_58 = 0;
    var c0_var_x_59 = 0;
    var c0_var_x_6 = 0;
    var c0_var_x_60 = 0;
    var c0_var_x_61 = 0;
    var c0_var_x_62 = 0;
    var c0_var_x_63 = 0;
    var c0_var_x_64 = 0;
    var c0_var_x_65 = 0;
    var c0_var_x_66 = 0;
    var c0_var_x_67 = 0;
    var c0_var_x_68 = 0;
    var c0_var_x_69 = 0;
    var c0_var_x_7 = 0;
    var c0_var_x_70 = 0;
    var c0_var_x_71 = 0;
    var c0_var_x_72 = 0;
    var c0_var_x_73 = 0;
    var c0_var_x_74 = 0;
    var c0_var_x_75 = 0;
    var c0_var_x_76 = 0;
    var c0_var_x_77 = 0;
    var c0_var_x_78 = 0;
    var c0_var_x_79 = 0;
    var c0_var_x_8 = 0;
    var c0_var_x_80 = 0;
    var c0_var_x_81 = 0;
    var c0_var_x_82 = 0;
    var c0_var_x_83 = 0;
    var c0_var_x_84 = 0;
    var c0_var_x_85 = 0;
    var c0_var_x_86 = 0;
    var c0_var_x_87 = 0;
    var c0_var_x_88 = 0;
    var c0_var_x_89 = 0;
    var c0_var_x_9 = 0;
    var c0_var_x_90 = 0;
    var c0_var_x_91 = 0;
    var c0_var_x_92 = 0;
    var c0_var_x_93 = 0;
    var c0_var_x_94 = 0;
    var c0_var_x_95 = 0;
    var c0_var_x_96 = 0;
    var c0_var_x_97 = 0;
    var c0_var_x_98 = 0;
    var c0_var_x_99 = 0;
     c0_var_x_0  =  1304242069 | 0;
     c0_var_x_1  =  1435960349 | 0;
     c0_var_x_2  =  1888836985 | 0;
     c0_var_x_3  =  290526982 | 0;
     c0_var_x_4  =  354156745 | 0;
     c0_var_x_5  =  1141125299 | 0;
     c0_var_x_6  =  1062732173 | 0;
     c0_var_x_7  =  1006006884 | 0;
     c0_var_x_8  =  1025428491 | 0;
     c0_var_x_9  =  559396644 | 0;
     c0_var_x_10  =  1456139964 | 0;
     c0_var_x_11  =  1940725939 | 0;
     c0_var_x_12  =  1732694457 | 0;
     c0_var_x_13  =  2027031487 | 0;
     c0_var_x_14  =  390598710 | 0;
     c0_var_x_15  =  987882812 | 0;
     c0_var_x_16  =  1514463777 | 0;
     c0_var_x_17  =  1571596537 | 0;
     c0_var_x_18  =  565290928 | 0;
     c0_var_x_19  =  682758957 | 0;
     c0_var_x_20  =  1845792473 | 0;
     c0_var_x_21  =  489820189 | 0;
     c0_var_x_22  =  1194643878 | 0;
     c0_var_x_23  =  769339795 | 0;
     c0_var_x_24  =  681615860 | 0;
     c0_var_x_25  =  1935723091 | 0;
     c0_var_x_26  =  607478070 | 0;
     c0_var_x_27  =  1956094671 | 0;
     c0_var_x_28  =  1038841569 | 0;
     c0_var_x_29  =  1466905652 | 0;
     c0_var_x_30  =  783428422 | 0;
     c0_var_x_31  =  1782531582 | 0;
     c0_var_x_32  =  1066993474 | 0;
     c0_var_x_33  =  929029912 | 0;
     c0_var_x_34  =  830459782 | 0;
     c0_var_x_35  =  1908394022 | 0;
     c0_var_x_36  =  1758199866 | 0;
     c0_var_x_37  =  208345926 | 0;
     c0_var_x_38  =  1380822714 | 0;
     c0_var_x_39  =  621838520 | 0;
     c0_var_x_40  =  831661907 | 0;
     c0_var_x_41  =  2021521275 | 0;
     c0_var_x_42  =  1408619007 | 0;
     c0_var_x_43  =  2016494323 | 0;
     c0_var_x_44  =  1728694352 | 0;
     c0_var_x_45  =  1635970503 | 0;
     c0_var_x_46  =  933906976 | 0;
     c0_var_x_47  =  1827464096 | 0;
     c0_var_x_48  =  2088997765 | 0;
     c0_var_x_49  =  575648811 | 0;
     c0_var_x_50  =  1741888816 | 0;
     c0_var_x_51  =  1211021139 | 0;
     c0_var_x_52  =  2087865165 | 0;
     c0_var_x_53  =  2086578393 | 0;
     c0_var_x_54  =  1402077625 | 0;
     c0_var_x_55  =  1173686425 | 0;
     c0_var_x_56  =  1171081592 | 0;
     c0_var_x_57  =  1737161149 | 0;
     c0_var_x_58  =  362047962 | 0;
     c0_var_x_59  =  586317128 | 0;
     c0_var_x_60  =  453644815 | 0;
     c0_var_x_61  =  76145348 | 0;
     c0_var_x_62  =  252850721 | 0;
     c0_var_x_63  =  1774450233 | 0;
     c0_var_x_64  =  1291330459 | 0;
     c0_var_x_65  =  534011409 | 0;
     c0_var_x_66  =  833783613 | 0;
     c0_var_x_67  =  1196290358 | 0;
     c0_var_x_68  =  2052912658 | 0;
     c0_var_x_69  =  326200202 | 0;
     c0_var_x_70  =  1889143847 | 0;
     c0_var_x_71  =  936307797 | 0;
     c0_var_x_72  =  1447206163 | 0;
     c0_var_x_73  =  1261009447 | 0;
     c0_var_x_74  =  576719347 | 0;
     c0_var_x_75  =  380176096 | 0;
     c0_var_x_76  =  1947841945 | 0;
     c0_var_x_77  =  562855520 | 0;
     c0_var_x_78  =  1874305482 | 0;
     c0_var_x_79  =  1445322368 | 0;
     c0_var_x_80  =  1908303333 | 0;
     c0_var_x_81  =  801916868 | 0;
     c0_var_x_82  =  1079364211 | 0;
     c0_var_x_83  =  953616254 | 0;
     c0_var_x_84  =  438873632 | 0;
     c0_var_x_85  =  1217576016 | 0;
     c0_var_x_86  =  886639932 | 0;
     c0_var_x_87  =  1738084770 | 0;
     c0_var_x_88  =  108572150 | 0;
     c0_var_x_89  =  805501850 | 0;
     c0_var_x_90  =  1057161749 | 0;
     c0_var_x_91  =  868820189 | 0;
     c0_var_x_92  =  1709504193 | 0;
     c0_var_x_93  =  15181311 | 0;
     c0_var_x_94  =  1515800615 | 0;
     c0_var_x_95  =  1788861857 | 0;
     c0_var_x_96  =  1329444924 | 0;
     c0_var_x_97  =  68668407 | 0;
     c0_var_x_98  =  1580507015 | 0;
     c0_var_x_99  =  1089199091 | 0;
     c0_var_poop  = ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( 1304242069  | 0) + ( 1435960349  | 0) | 0) + ( 1888836985  | 0) | 0) + ( 290526982  | 0) | 0) + ( 354156745  | 0) | 0) + ( 1141125299  | 0) | 0) + ( 1062732173  | 0) | 0) + ( 1006006884  | 0) | 0) + ( 1025428491  | 0) | 0) + ( 559396644  | 0) | 0) + ( 1456139964  | 0) | 0) + ( 1940725939  | 0) | 0) + ( 1732694457  | 0) | 0) + ( 2027031487  | 0) | 0) + ( 390598710  | 0) | 0) + ( 987882812  | 0) | 0) + ( 1514463777  | 0) | 0) + ( 1571596537  | 0) | 0) + ( 565290928  | 0) | 0) + ( 682758957  | 0) | 0) + ( 1845792473  | 0) | 0) + ( 489820189  | 0) | 0) + ( 1194643878  | 0) | 0) + ( 769339795  | 0) | 0) + ( 681615860  | 0) | 0) + ( 1935723091  | 0) | 0) + ( 607478070  | 0) | 0) + ( 1956094671  | 0) | 0) + ( 1038841569  | 0) | 0) + ( 1466905652  | 0) | 0) + ( 783428422  | 0) | 0) + ( 1782531582  | 0) | 0) + ( 1066993474  | 0) | 0) + ( 929029912  | 0) | 0) + ( 830459782  | 0) | 0) + ( 1908394022  | 0) | 0) + ( 1758199866  | 0) | 0) + ( 208345926  | 0) | 0) + ( 1380822714  | 0) | 0) + ( 621838520  | 0) | 0) + ( 831661907  | 0) | 0) + ( 2021521275  | 0) | 0) + ( 1408619007  | 0) | 0) + ( 2016494323  | 0) | 0) + ( 1728694352  | 0) | 0) + ( 1635970503  | 0) | 0) + ( 933906976  | 0) | 0) + ( 1827464096  | 0) | 0) + ( 2088997765  | 0) | 0) + ( 575648811  | 0) | 0) + ( 1741888816  | 0) | 0) + ( 1211021139  | 0) | 0) + ( 2087865165  | 0) | 0) + ( 2086578393  | 0) | 0) + ( 1402077625  | 0) | 0) + ( 1173686425  | 0) | 0) + ( 1171081592  | 0) | 0) + ( 1737161149  | 0) | 0) + ( 362047962  | 0) | 0) + ( 586317128  | 0) | 0) + ( 453644815  | 0) | 0) + ( 76145348  | 0) | 0) + ( 252850721  | 0) | 0) + ( 1774450233  | 0) | 0) + ( 1291330459  | 0) | 0) + ( 534011409  | 0) | 0) + ( 833783613  | 0) | 0) + ( 1196290358  | 0) | 0) + ( 2052912658  | 0) | 0) + ( 326200202  | 0) | 0) + ( 1889143847  | 0) | 0) + ( 936307797  | 0) | 0) + ( 1447206163  | 0) | 0) + ( 1261009447  | 0) | 0) + ( 576719347  | 0) | 0) + ( 380176096  | 0) | 0) + ( 1947841945  | 0) | 0) + ( 562855520  | 0) | 0) + ( 1874305482  | 0) | 0) + ( 1445322368  | 0) | 0) + ( 1908303333  | 0) | 0) + ( 801916868  | 0) | 0) + ( 1079364211  | 0) | 0) + ( 953616254  | 0) | 0) + ( 438873632  | 0) | 0) + ( 1217576016  | 0) | 0) + ( 886639932  | 0) | 0) + ( 1738084770  | 0) | 0) + ( 108572150  | 0) | 0) + ( 805501850  | 0) | 0) + ( 1057161749  | 0) | 0) + ( 868820189  | 0) | 0) + ( 1709504193  | 0) | 0) + ( 15181311  | 0) | 0) + ( 1515800615  | 0) | 0) + ( 1788861857  | 0) | 0) + ( 1329444924  | 0) | 0) + ( 68668407  | 0) | 0) + ( 1580507015  | 0) | 0) + ( 1089199091  | 0)| 0;
    return ( c0_var_poop  | 0);

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