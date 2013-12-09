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
    var c0_var_AAB = 0;
    var c0_var_AAC = 0;
    var c0_var_AB = 0;
    var c0_var_ABB = 0;
    var c0_var_ABC = 0;
    var c0_var_AC = 0;
    var c0_var_ACB = 0;
    var c0_var_ACC = 0;
    var c0_var_AD = 0;
    var c0_var_ADB = 0;
    var c0_var_ADC = 0;
    var c0_var_AE = 0;
    var c0_var_AEB = 0;
    var c0_var_AEC = 0;
    var c0_var_AF = 0;
    var c0_var_AFB = 0;
    var c0_var_AFC = 0;
    var c0_var_AG = 0;
    var c0_var_AGB = 0;
    var c0_var_AH = 0;
    var c0_var_AHB = 0;
    var c0_var_AI = 0;
    var c0_var_AIB = 0;
    var c0_var_AJ = 0;
    var c0_var_AJB = 0;
    var c0_var_B = 0;
    var c0_var_BAB = 0;
    var c0_var_BAC = 0;
    var c0_var_BB = 0;
    var c0_var_BBB = 0;
    var c0_var_BBC = 0;
    var c0_var_BC = 0;
    var c0_var_BCB = 0;
    var c0_var_BCC = 0;
    var c0_var_BD = 0;
    var c0_var_BDB = 0;
    var c0_var_BDC = 0;
    var c0_var_BE = 0;
    var c0_var_BEB = 0;
    var c0_var_BEC = 0;
    var c0_var_BF = 0;
    var c0_var_BFB = 0;
    var c0_var_BG = 0;
    var c0_var_BGB = 0;
    var c0_var_BH = 0;
    var c0_var_BHB = 0;
    var c0_var_BI = 0;
    var c0_var_BIB = 0;
    var c0_var_BJ = 0;
    var c0_var_BJB = 0;
    var c0_var_C = 0;
    var c0_var_CAB = 0;
    var c0_var_CAC = 0;
    var c0_var_CB = 0;
    var c0_var_CBB = 0;
    var c0_var_CBC = 0;
    var c0_var_CC = 0;
    var c0_var_CCB = 0;
    var c0_var_CCC = 0;
    var c0_var_CD = 0;
    var c0_var_CDB = 0;
    var c0_var_CDC = 0;
    var c0_var_CE = 0;
    var c0_var_CEB = 0;
    var c0_var_CEC = 0;
    var c0_var_CF = 0;
    var c0_var_CFB = 0;
    var c0_var_CG = 0;
    var c0_var_CGB = 0;
    var c0_var_CH = 0;
    var c0_var_CHB = 0;
    var c0_var_CI = 0;
    var c0_var_CIB = 0;
    var c0_var_CJ = 0;
    var c0_var_CJB = 0;
    var c0_var_D = 0;
    var c0_var_DAB = 0;
    var c0_var_DAC = 0;
    var c0_var_DB = 0;
    var c0_var_DBB = 0;
    var c0_var_DBC = 0;
    var c0_var_DC = 0;
    var c0_var_DCB = 0;
    var c0_var_DCC = 0;
    var c0_var_DD = 0;
    var c0_var_DDB = 0;
    var c0_var_DDC = 0;
    var c0_var_DE = 0;
    var c0_var_DEB = 0;
    var c0_var_DEC = 0;
    var c0_var_DF = 0;
    var c0_var_DFB = 0;
    var c0_var_DG = 0;
    var c0_var_DGB = 0;
    var c0_var_DH = 0;
    var c0_var_DHB = 0;
    var c0_var_DI = 0;
    var c0_var_DIB = 0;
    var c0_var_DJ = 0;
    var c0_var_DJB = 0;
    var c0_var_E = 0;
    var c0_var_EAB = 0;
    var c0_var_EAC = 0;
    var c0_var_EB = 0;
    var c0_var_EBB = 0;
    var c0_var_EBC = 0;
    var c0_var_EC = 0;
    var c0_var_ECB = 0;
    var c0_var_ECC = 0;
    var c0_var_ED = 0;
    var c0_var_EDB = 0;
    var c0_var_EDC = 0;
    var c0_var_EE = 0;
    var c0_var_EEB = 0;
    var c0_var_EEC = 0;
    var c0_var_EF = 0;
    var c0_var_EFB = 0;
    var c0_var_EG = 0;
    var c0_var_EGB = 0;
    var c0_var_EH = 0;
    var c0_var_EHB = 0;
    var c0_var_EI = 0;
    var c0_var_EIB = 0;
    var c0_var_EJ = 0;
    var c0_var_EJB = 0;
    var c0_var_F = 0;
    var c0_var_FAB = 0;
    var c0_var_FAC = 0;
    var c0_var_FB = 0;
    var c0_var_FBB = 0;
    var c0_var_FBC = 0;
    var c0_var_FC = 0;
    var c0_var_FCB = 0;
    var c0_var_FCC = 0;
    var c0_var_FD = 0;
    var c0_var_FDB = 0;
    var c0_var_FDC = 0;
    var c0_var_FE = 0;
    var c0_var_FEB = 0;
    var c0_var_FEC = 0;
    var c0_var_FF = 0;
    var c0_var_FFB = 0;
    var c0_var_FG = 0;
    var c0_var_FGB = 0;
    var c0_var_FH = 0;
    var c0_var_FHB = 0;
    var c0_var_FI = 0;
    var c0_var_FIB = 0;
    var c0_var_FJ = 0;
    var c0_var_FJB = 0;
    var c0_var_G = 0;
    var c0_var_GAB = 0;
    var c0_var_GAC = 0;
    var c0_var_GB = 0;
    var c0_var_GBB = 0;
    var c0_var_GBC = 0;
    var c0_var_GC = 0;
    var c0_var_GCB = 0;
    var c0_var_GCC = 0;
    var c0_var_GD = 0;
    var c0_var_GDB = 0;
    var c0_var_GDC = 0;
    var c0_var_GE = 0;
    var c0_var_GEB = 0;
    var c0_var_GEC = 0;
    var c0_var_GF = 0;
    var c0_var_GFB = 0;
    var c0_var_GG = 0;
    var c0_var_GGB = 0;
    var c0_var_GH = 0;
    var c0_var_GHB = 0;
    var c0_var_GI = 0;
    var c0_var_GIB = 0;
    var c0_var_GJ = 0;
    var c0_var_GJB = 0;
    var c0_var_H = 0;
    var c0_var_HAB = 0;
    var c0_var_HAC = 0;
    var c0_var_HB = 0;
    var c0_var_HBB = 0;
    var c0_var_HBC = 0;
    var c0_var_HC = 0;
    var c0_var_HCB = 0;
    var c0_var_HCC = 0;
    var c0_var_HD = 0;
    var c0_var_HDB = 0;
    var c0_var_HDC = 0;
    var c0_var_HE = 0;
    var c0_var_HEB = 0;
    var c0_var_HEC = 0;
    var c0_var_HF = 0;
    var c0_var_HFB = 0;
    var c0_var_HG = 0;
    var c0_var_HGB = 0;
    var c0_var_HH = 0;
    var c0_var_HHB = 0;
    var c0_var_HI = 0;
    var c0_var_HIB = 0;
    var c0_var_HJ = 0;
    var c0_var_HJB = 0;
    var c0_var_I = 0;
    var c0_var_IAB = 0;
    var c0_var_IAC = 0;
    var c0_var_IB = 0;
    var c0_var_IBB = 0;
    var c0_var_IBC = 0;
    var c0_var_IC = 0;
    var c0_var_ICB = 0;
    var c0_var_ICC = 0;
    var c0_var_ID = 0;
    var c0_var_IDB = 0;
    var c0_var_IDC = 0;
    var c0_var_IE = 0;
    var c0_var_IEB = 0;
    var c0_var_IEC = 0;
    var c0_var_IF = 0;
    var c0_var_IFB = 0;
    var c0_var_IG = 0;
    var c0_var_IGB = 0;
    var c0_var_IH = 0;
    var c0_var_IHB = 0;
    var c0_var_II = 0;
    var c0_var_IIB = 0;
    var c0_var_IJ = 0;
    var c0_var_IJB = 0;
    var c0_var_J = 0;
    var c0_var_JAB = 0;
    var c0_var_JAC = 0;
    var c0_var_JB = 0;
    var c0_var_JBB = 0;
    var c0_var_JBC = 0;
    var c0_var_JC = 0;
    var c0_var_JCB = 0;
    var c0_var_JCC = 0;
    var c0_var_JD = 0;
    var c0_var_JDB = 0;
    var c0_var_JDC = 0;
    var c0_var_JE = 0;
    var c0_var_JEB = 0;
    var c0_var_JEC = 0;
    var c0_var_JF = 0;
    var c0_var_JFB = 0;
    var c0_var_JG = 0;
    var c0_var_JGB = 0;
    var c0_var_JH = 0;
    var c0_var_JHB = 0;
    var c0_var_JI = 0;
    var c0_var_JIB = 0;
    var c0_var_JJ = 0;
    var c0_var_JJB = 0;
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
     c0_var_BB  = imul( 4914  | 0,( 1528  | 0) - ((( 6437  | 0) - ( 3368  | 0) | 0) - ( 6029  | 0) | 0) | 0) | 0| 0;
     c0_var_CB  =  5818 | 0;
     c0_var_G  = ( c0_var_G  | 0) - (imul(imul(((imul( 5763  | 0, 3368  | 0) | 0 | 0) - ( 688  | 0) | 0) + ( 6996  | 0) | 0, 5818  | 0) | 0 | 0, 6725  | 0) | 0 | 0)| 0;
     c0_var_DB  =  c0_var_BB | 0;
     c0_var_CB  = imul( c0_var_CB  | 0, c0_var_CB  | 0) | 0| 0;
     c0_var_EB  =  8611 | 0;
     c0_var_FB  =  c0_var_G | 0;
     c0_var_B  = ( c0_var_B  | 0) - ( 5422  | 0)| 0;
     c0_var_GB  =  c0_var_B | 0;
     c0_var_HB  =  8736 | 0;
     c0_var_IB  =  3465 | 0;
     c0_var_H  = ( c0_var_H  | 0) + ( 3258  | 0)| 0;
     c0_var_JB  =  c0_var_DB | 0;
     c0_var_AC  =  5550 | 0;
     c0_var_BC  =  c0_var_CB | 0;
     c0_var_F  = imul( c0_var_F  | 0, 5894  | 0) | 0| 0;
     c0_var_CC  = (( c0_var_H  | 0) - ((((( 1340  | 0) - ((imul(( 7445  | 0) + (( c0_var_CB  | 0) - ( c0_var_DB  | 0) | 0) | 0, 7587  | 0) | 0 | 0) + ( c0_var_H  | 0) | 0) | 0) - ( 8611  | 0) | 0) + ((imul( 5667  | 0, c0_var_FB  | 0) | 0 | 0) + ( c0_var_BB  | 0) | 0) | 0) - ( 8736  | 0) | 0) | 0) + ( c0_var_JB  | 0)| 0;
     c0_var_J  = imul( c0_var_J  | 0, 5385  | 0) | 0| 0;
     c0_var_DC  =  713 | 0;
     c0_var_BC  = ( c0_var_BC  | 0) + ( 2590  | 0)| 0;
     c0_var_EC  =  1155 | 0;
     c0_var_FC  =  3465 | 0;
     c0_var_GC  = ( c0_var_BB  | 0) - ( c0_var_JB  | 0)| 0;
     c0_var_FB  = ( c0_var_FB  | 0) + ( c0_var_J  | 0)| 0;
     c0_var_FB  = ( c0_var_FB  | 0) - ( 8944  | 0)| 0;
     c0_var_CB  =  2842 | 0;
     c0_var_FB  =  9958 | 0;
     c0_var_HC  =  1528 | 0;
     c0_var_IC  =  2890 | 0;
     c0_var_HC  =  8736 | 0;
     c0_var_BC  = imul( c0_var_BC  | 0,( c0_var_JB  | 0) - (( c0_var_CC  | 0) - ( 1152  | 0) | 0) | 0) | 0| 0;
     c0_var_JC  =  5436 | 0;
     c0_var_CB  =  8611 | 0;
     c0_var_IB  = ( c0_var_IB  | 0) - ( 713  | 0)| 0;
     c0_var_AD  =  1039 | 0;
     c0_var_BD  =  4500 | 0;
     c0_var_B  = imul( c0_var_B  | 0, 5436  | 0) | 0| 0;
     c0_var_CC  =  2215 | 0;
     c0_var_CD  = imul( c0_var_GB  | 0, 5436  | 0) | 0| 0;
     c0_var_F  =  c0_var_C | 0;
     c0_var_FC  = ( c0_var_FC  | 0) - (( 8611  | 0) - ( c0_var_G  | 0) | 0)| 0;
     c0_var_DD  =  15426 | 0;
     c0_var_ED  =  9201 | 0;
     c0_var_FD  =  1555868 | 0;
     c0_var_G  = ( c0_var_G  | 0) - (imul((( 8946  | 0) - (((( 1155  | 0) - ( 1886  | 0) | 0) + ( 90  | 0) | 0) + ( 9958  | 0) | 0) | 0) + ( 7641  | 0) | 0, c0_var_JB  | 0) | 0 | 0)| 0;
     c0_var_GD  =  417 | 0;
     c0_var_IB  = imul( c0_var_IB  | 0, c0_var_IB  | 0) | 0| 0;
     c0_var_HD  =  c0_var_E | 0;
     c0_var_ID  = ( c0_var_HD  | 0) - (( -11633  | 0) - ( c0_var_GB  | 0) | 0)| 0;
     c0_var_JD  = ( 4292  | 0) - ( c0_var_ID  | 0)| 0;
     c0_var_AE  =  1947 | 0;
     c0_var_BD  = ( c0_var_BD  | 0) - ( c0_var_IB  | 0)| 0;
     c0_var_BE  =  1081 | 0;
     c0_var_CE  =  1528 | 0;
     c0_var_ED  = ( c0_var_ED  | 0) - ( 2338  | 0)| 0;
     c0_var_DE  = ( 2379  | 0) + ( 9904  | 0)| 0;
     c0_var_IB  = ( c0_var_D  | 0) - ( c0_var_BC  | 0)| 0;
     c0_var_EE  =  1081 | 0;
     c0_var_G  = ( 9934  | 0) - ( 1555868  | 0)| 0;
     c0_var_FE  =  9958 | 0;
     c0_var_FC  = imul( c0_var_FC  | 0, 4596198  | 0) | 0| 0;
     c0_var_GE  =  c0_var_FE | 0;
     c0_var_B  = ( c0_var_B  | 0) - ( 7088  | 0)| 0;
     c0_var_HE  = ( 235  | 0) - ( c0_var_CB  | 0)| 0;
     c0_var_I  = ( c0_var_I  | 0) - ( 9291  | 0)| 0;
     c0_var_IE  =  5055 | 0;
     c0_var_JE  =  4904 | 0;
     c0_var_AD  = ( c0_var_AD  | 0) + ( 9685  | 0)| 0;
     c0_var_IE  = imul( c0_var_IE  | 0, 9250  | 0) | 0| 0;
     c0_var_GE  =  4639 | 0;
     c0_var_DB  = ( c0_var_DB  | 0) + ( c0_var_FE  | 0)| 0;
     c0_var_DB  = ( c0_var_DB  | 0) - ((( 2215  | 0) + (( 5550  | 0) + (( 4427  | 0) + ( 2215  | 0) | 0) | 0) | 0) - ( c0_var_EE  | 0) | 0)| 0;
     c0_var_JE  =  c0_var_IB | 0;
     c0_var_AF  =  c0_var_DB | 0;
     c0_var_BF  =  c0_var_HD | 0;
     c0_var_CF  =  c0_var_D | 0;
     c0_var_DF  =  9848 | 0;
     c0_var_GD  = imul( c0_var_GD  | 0,( 4154  | 0) + ( c0_var_GD  | 0) | 0) | 0| 0;
     c0_var_C  = ( c0_var_C  | 0) + ( 8701  | 0)| 0;
     c0_var_JB  = ( c0_var_JB  | 0) + ( 6728  | 0)| 0;
     c0_var_H  = imul( c0_var_H  | 0, c0_var_J  | 0) | 0| 0;
     c0_var_E  = ( c0_var_E  | 0) + ( 1052  | 0)| 0;
     c0_var_B  = ( c0_var_B  | 0) + ( 9016  | 0)| 0;
     c0_var_HB  = ( c0_var_HB  | 0) + ( c0_var_D  | 0)| 0;
     c0_var_GE  = imul( c0_var_GE  | 0, 9426  | 0) | 0| 0;
     c0_var_EF  =  1943 | 0;
     c0_var_FF  =  2215 | 0;
     c0_var_GF  =  9181 | 0;
     c0_var_HF  =  2433 | 0;
     c0_var_IF  =  c0_var_CB | 0;
     c0_var_JF  =  8722 | 0;
     c0_var_FD  = imul( c0_var_FD  | 0, 16191  | 0) | 0| 0;
     c0_var_AG  =  5550 | 0;
     c0_var_DE  = ( 2890  | 0) + ( 4266  | 0)| 0;
     c0_var_BD  = ( c0_var_BD  | 0) - ( 9086  | 0)| 0;
     c0_var_BG  = ( c0_var_HB  | 0) - (imul( 8839  | 0,(imul( 7643  | 0,( c0_var_BD  | 0) - ( c0_var_GD  | 0) | 0) | 0 | 0) - ( c0_var_JE  | 0) | 0) | 0 | 0)| 0;
     c0_var_CG  =  c0_var_AG | 0;
     c0_var_GF  = imul( c0_var_GF  | 0, c0_var_IE  | 0) | 0| 0;
     c0_var_AD  = ( 8398  | 0) + ( c0_var_GD  | 0)| 0;
     c0_var_EE  = imul( c0_var_EE  | 0, 6087  | 0) | 0| 0;
     c0_var_HC  = imul( c0_var_HC  | 0, c0_var_EE  | 0) | 0| 0;
     c0_var_IE  = ( c0_var_IE  | 0) + (imul( 6776  | 0,( 786940  | 0) - (imul( c0_var_CG  | 0, 6127  | 0) | 0 | 0) | 0) | 0 | 0)| 0;
     c0_var_DG  =  7334 | 0;
     c0_var_EG  =  2890 | 0;
     c0_var_BG  =  c0_var_FE | 0;
     c0_var_D  = ( c0_var_D  | 0) + ( 8722  | 0)| 0;
     c0_var_FG  = ( c0_var_EE  | 0) + ( 2491  | 0)| 0;
     c0_var_GB  = imul( c0_var_GB  | 0, c0_var_FF  | 0) | 0| 0;
     c0_var_GG  =  6590 | 0;
     c0_var_HG  =  c0_var_GF | 0;
     c0_var_BB  = imul( c0_var_BB  | 0, 7361  | 0) | 0| 0;
     c0_var_HG  = imul(( c0_var_HD  | 0) + (( 7334  | 0) + ( 2538  | 0) | 0) | 0, 1956  | 0) | 0| 0;
     c0_var_IG  =  c0_var_D | 0;
     c0_var_HD  = imul( c0_var_HD  | 0, 6039  | 0) | 0| 0;
     c0_var_JG  =  1790 | 0;
     c0_var_AH  =  c0_var_HE | 0;
     c0_var_BH  =  4295 | 0;
     c0_var_CH  =  3198 | 0;
     c0_var_DH  =  7793 | 0;
     c0_var_IC  = ( c0_var_IC  | 0) - ( c0_var_G  | 0)| 0;
     c0_var_EG  = ( c0_var_EG  | 0) + ( 140  | 0)| 0;
     c0_var_IG  = ( c0_var_IG  | 0) - ( 50  | 0)| 0;
     c0_var_EH  =  c0_var_AG | 0;
     c0_var_FH  = imul( 3469  | 0, c0_var_ID  | 0) | 0| 0;
     c0_var_GH  =  c0_var_FF | 0;
     c0_var_F  = imul( c0_var_F  | 0, 3666  | 0) | 0| 0;
     c0_var_CH  = ( 2433  | 0) - ( 9207  | 0)| 0;
     c0_var_FF  = ( c0_var_FF  | 0) + ( 7472  | 0)| 0;
     c0_var_IF  = ( c0_var_IF  | 0) - ( 6797  | 0)| 0;
     c0_var_GE  = ( c0_var_GE  | 0) + ( c0_var_BB  | 0)| 0;
     c0_var_BH  = imul( c0_var_BH  | 0, 30  | 0) | 0| 0;
     c0_var_GE  =  6424 | 0;
     c0_var_HH  = imul( c0_var_BB  | 0, 15426  | 0) | 0| 0;
     c0_var_D  = ( c0_var_D  | 0) - ( 15426  | 0)| 0;
     c0_var_IH  =  9613 | 0;
     c0_var_JH  =  16410 | 0;
     c0_var_AI  =  7088 | 0;
     c0_var_BI  =  8232 | 0;
     c0_var_CI  =  30 | 0;
     c0_var_DI  =  7279 | 0;
     c0_var_EI  =  2351 | 0;
     c0_var_CE  = imul( c0_var_CE  | 0, 7330  | 0) | 0| 0;
     c0_var_FI  = imul( c0_var_IC  | 0, c0_var_BF  | 0) | 0| 0;
     c0_var_GI  =  1519 | 0;
     c0_var_HI  =  3124 | 0;
     c0_var_II  =  713 | 0;
     c0_var_CD  = ( c0_var_CD  | 0) + ( c0_var_EH  | 0)| 0;
     c0_var_JE  = ( c0_var_JE  | 0) + ((( 1087  | 0) + ( c0_var_GF  | 0) | 0) - ( c0_var_C  | 0) | 0)| 0;
     c0_var_JI  =  c0_var_ID | 0;
     c0_var_AJ  =  1790 | 0;
     c0_var_JH  = ( c0_var_JH  | 0) + ( 2443  | 0)| 0;
     c0_var_ID  = ( c0_var_ID  | 0) - ( c0_var_BD  | 0)| 0;
     c0_var_BJ  =  2589 | 0;
     c0_var_CJ  = imul( 4794  | 0, c0_var_FI  | 0) | 0| 0;
     c0_var_DJ  =  8820 | 0;
     c0_var_AG  = ( c0_var_AG  | 0) + ( 6354  | 0)| 0;
     c0_var_EJ  =  c0_var_BF | 0;
     c0_var_GB  =  c0_var_GD | 0;
     c0_var_FJ  =  7422 | 0;
     c0_var_GJ  =  1947 | 0;
     c0_var_HJ  =  6571 | 0;
     c0_var_IJ  =  c0_var_AJ | 0;
     c0_var_JH  =  3566 | 0;
     c0_var_GG  = ( c0_var_GG  | 0) - ( 3333  | 0)| 0;
     c0_var_JJ  =  c0_var_HC | 0;
     c0_var_AAB  =  2639 | 0;
     c0_var_EB  = ( c0_var_EB  | 0) - ( 3590  | 0)| 0;
     c0_var_BAB  =  8257 | 0;
     c0_var_BH  = ( c0_var_BH  | 0) - (( c0_var_EE  | 0) + ( 6421  | 0) | 0)| 0;
     c0_var_CAB  =  356 | 0;
     c0_var_CB  = imul( c0_var_CB  | 0, 3446  | 0) | 0| 0;
     c0_var_IB  =  5825 | 0;
     c0_var_F  = imul( c0_var_F  | 0, 596  | 0) | 0| 0;
     c0_var_B  = ( c0_var_B  | 0) - ( 519  | 0)| 0;
     c0_var_DAB  =  5550 | 0;
     c0_var_CC  = imul( c0_var_CC  | 0, c0_var_E  | 0) | 0| 0;
     c0_var_EAB  =  4976 | 0;
     c0_var_IB  =  83 | 0;
     c0_var_FAB  =  5550 | 0;
     c0_var_GAB  =  1943 | 0;
     c0_var_HI  = imul( c0_var_HI  | 0, 2251  | 0) | 0| 0;
     c0_var_FD  = imul( c0_var_FD  | 0, 19047726  | 0) | 0| 0;
     c0_var_B  = ( c0_var_B  | 0) + ( c0_var_IJ  | 0)| 0;
     c0_var_II  = ( c0_var_II  | 0) + ( 9819  | 0)| 0;
     c0_var_FI  = ( c0_var_FI  | 0) - ( 7639  | 0)| 0;
     c0_var_HAB  =  7334 | 0;
     c0_var_IAB  = ( 6463  | 0) + ( 83  | 0)| 0;
     c0_var_FJ  =  8098 | 0;
     c0_var_FE  = imul( c0_var_FE  | 0, 4797  | 0) | 0| 0;
     c0_var_J  = ( c0_var_J  | 0) + ( 9912  | 0)| 0;
     c0_var_JAB  = ( 2351  | 0) - ( 8498  | 0)| 0;
     c0_var_ABB  =  c0_var_IF | 0;
     c0_var_BBB  =  c0_var_FH | 0;
     c0_var_FF  =  3633 | 0;
     c0_var_CBB  =  c0_var_EJ | 0;
     c0_var_DBB  = imul( 1943  | 0, c0_var_DB  | 0) | 0| 0;
     c0_var_G  = ( c0_var_G  | 0) - ( 2765  | 0)| 0;
     c0_var_D  = ( c0_var_D  | 0) + ( 11084  | 0)| 0;
     c0_var_BH  = ( 5026  | 0) - ( c0_var_GB  | 0)| 0;
     c0_var_EBB  =  775 | 0;
     c0_var_C  = ( c0_var_C  | 0) - ( 3320  | 0)| 0;
     c0_var_IAB  =  c0_var_BBB | 0;
     c0_var_FBB  = ( 17266  | 0) - ( 7334  | 0)| 0;
     c0_var_GBB  = (imul( c0_var_HH  | 0, 4187  | 0) | 0 | 0) + ((( c0_var_FC  | 0) - ( c0_var_FG  | 0) | 0) + ( 3162  | 0) | 0)| 0;
     c0_var_HBB  =  4163 | 0;
     c0_var_BE  = ( c0_var_BE  | 0) + (( c0_var_BF  | 0) + ( c0_var_DE  | 0) | 0)| 0;
     c0_var_IBB  = ( c0_var_BE  | 0) + ( 15426  | 0)| 0;
     c0_var_JBB  =  6951 | 0;
     c0_var_ACB  =  c0_var_FE | 0;
     c0_var_FF  = imul( c0_var_FF  | 0, 2499  | 0) | 0| 0;
     c0_var_IF  = imul( c0_var_IF  | 0, 12518  | 0) | 0| 0;
     c0_var_FB  = imul( 3281  | 0, c0_var_CB  | 0) | 0| 0;
     c0_var_BCB  =  3567 | 0;
     c0_var_CCB  =  3674 | 0;
     c0_var_IBB  = ( c0_var_IBB  | 0) + ( 7457  | 0)| 0;
     c0_var_DCB  =  5292 | 0;
     c0_var_DJ  =  c0_var_HB | 0;
     c0_var_ECB  =  339 | 0;
     c0_var_DD  =  1653 | 0;
     c0_var_FCB  = imul( 2983  | 0,((imul( c0_var_FB  | 0, c0_var_CB  | 0) | 0 | 0) + ( 9221  | 0) | 0) - ( c0_var_CH  | 0) | 0) | 0| 0;
     c0_var_GCB  =  4508736 | 0;
     c0_var_HCB  =  9503 | 0;
     c0_var_ICB  =  4057 | 0;
     c0_var_JCB  =  c0_var_DJ | 0;
     c0_var_ADB  =  8012 | 0;
     c0_var_BDB  =  c0_var_FBB | 0;
     c0_var_IB  = ( c0_var_IB  | 0) + ( c0_var_G  | 0)| 0;
    return ( c0_var_BDB  | 0);

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