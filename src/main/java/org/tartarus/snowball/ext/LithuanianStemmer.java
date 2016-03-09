// This file was generated automatically by the Snowball to Java compiler
// http://snowballstem.org/

package org.tartarus.snowball.ext;

import org.tartarus.snowball.Among;

 /**
  * This class was automatically generated by a Snowball to Java compiler
  * It implements the stemming algorithm defined by a snowball script.
  *
  * Taken from: https://github.com/dainiusjocas/snowball/blob/master/algorithms/lithuanian/stem_ISO_8859_1.sbl
  */

public class  LithuanianStemmer extends org.tartarus.snowball.SnowballProgram {

    private static final long serialVersionUID = 1L;

    private final static Among a_0[] = {
        new Among ( "a", -1, -1 ),
        new Among ( "ia", 0, -1 ),
        new Among ( "eria", 1, -1 ),
        new Among ( "osna", 0, -1 ),
        new Among ( "iosna", 3, -1 ),
        new Among ( "uosna", 3, -1 ),
        new Among ( "iuosna", 5, -1 ),
        new Among ( "ysna", 0, -1 ),
        new Among ( "\u0117sna", 0, -1 ),
        new Among ( "e", -1, -1 ),
        new Among ( "ie", 9, -1 ),
        new Among ( "enie", 10, -1 ),
        new Among ( "erie", 10, -1 ),
        new Among ( "oje", 9, -1 ),
        new Among ( "ioje", 13, -1 ),
        new Among ( "uje", 9, -1 ),
        new Among ( "iuje", 15, -1 ),
        new Among ( "yje", 9, -1 ),
        new Among ( "enyje", 17, -1 ),
        new Among ( "eryje", 17, -1 ),
        new Among ( "\u0117je", 9, -1 ),
        new Among ( "ame", 9, -1 ),
        new Among ( "iame", 21, -1 ),
        new Among ( "sime", 9, -1 ),
        new Among ( "ome", 9, -1 ),
        new Among ( "\u0117me", 9, -1 ),
        new Among ( "tum\u0117me", 25, -1 ),
        new Among ( "ose", 9, -1 ),
        new Among ( "iose", 27, -1 ),
        new Among ( "uose", 27, -1 ),
        new Among ( "iuose", 29, -1 ),
        new Among ( "yse", 9, -1 ),
        new Among ( "enyse", 31, -1 ),
        new Among ( "eryse", 31, -1 ),
        new Among ( "\u0117se", 9, -1 ),
        new Among ( "ate", 9, -1 ),
        new Among ( "iate", 35, -1 ),
        new Among ( "ite", 9, -1 ),
        new Among ( "kite", 37, -1 ),
        new Among ( "site", 37, -1 ),
        new Among ( "ote", 9, -1 ),
        new Among ( "tute", 9, -1 ),
        new Among ( "\u0117te", 9, -1 ),
        new Among ( "tum\u0117te", 42, -1 ),
        new Among ( "i", -1, -1 ),
        new Among ( "ai", 44, -1 ),
        new Among ( "iai", 45, -1 ),
        new Among ( "eriai", 46, -1 ),
        new Among ( "ei", 44, -1 ),
        new Among ( "tumei", 48, -1 ),
        new Among ( "ki", 44, -1 ),
        new Among ( "imi", 44, -1 ),
        new Among ( "erimi", 51, -1 ),
        new Among ( "umi", 44, -1 ),
        new Among ( "iumi", 53, -1 ),
        new Among ( "si", 44, -1 ),
        new Among ( "asi", 55, -1 ),
        new Among ( "iasi", 56, -1 ),
        new Among ( "esi", 55, -1 ),
        new Among ( "iesi", 58, -1 ),
        new Among ( "siesi", 59, -1 ),
        new Among ( "isi", 55, -1 ),
        new Among ( "aisi", 61, -1 ),
        new Among ( "eisi", 61, -1 ),
        new Among ( "tumeisi", 63, -1 ),
        new Among ( "uisi", 61, -1 ),
        new Among ( "osi", 55, -1 ),
        new Among ( "\u0117josi", 66, -1 ),
        new Among ( "uosi", 66, -1 ),
        new Among ( "iuosi", 68, -1 ),
        new Among ( "siuosi", 69, -1 ),
        new Among ( "usi", 55, -1 ),
        new Among ( "ausi", 71, -1 ),
        new Among ( "\u010Diausi", 72, -1 ),
        new Among ( "\u0105si", 55, -1 ),
        new Among ( "\u0117si", 55, -1 ),
        new Among ( "\u0173si", 55, -1 ),
        new Among ( "t\u0173si", 76, -1 ),
        new Among ( "ti", 44, -1 ),
        new Among ( "enti", 78, -1 ),
        new Among ( "inti", 78, -1 ),
        new Among ( "oti", 78, -1 ),
        new Among ( "ioti", 81, -1 ),
        new Among ( "uoti", 81, -1 ),
        new Among ( "iuoti", 83, -1 ),
        new Among ( "auti", 78, -1 ),
        new Among ( "iauti", 85, -1 ),
        new Among ( "yti", 78, -1 ),
        new Among ( "\u0117ti", 78, -1 ),
        new Among ( "tel\u0117ti", 88, -1 ),
        new Among ( "in\u0117ti", 88, -1 ),
        new Among ( "ter\u0117ti", 88, -1 ),
        new Among ( "ui", 44, -1 ),
        new Among ( "iui", 92, -1 ),
        new Among ( "eniui", 93, -1 ),
        new Among ( "oj", -1, -1 ),
        new Among ( "\u0117j", -1, -1 ),
        new Among ( "k", -1, -1 ),
        new Among ( "am", -1, -1 ),
        new Among ( "iam", 98, -1 ),
        new Among ( "iem", -1, -1 ),
        new Among ( "im", -1, -1 ),
        new Among ( "sim", 101, -1 ),
        new Among ( "om", -1, -1 ),
        new Among ( "tum", -1, -1 ),
        new Among ( "\u0117m", -1, -1 ),
        new Among ( "tum\u0117m", 105, -1 ),
        new Among ( "an", -1, -1 ),
        new Among ( "on", -1, -1 ),
        new Among ( "ion", 108, -1 ),
        new Among ( "un", -1, -1 ),
        new Among ( "iun", 110, -1 ),
        new Among ( "\u0117n", -1, -1 ),
        new Among ( "o", -1, -1 ),
        new Among ( "io", 113, -1 ),
        new Among ( "enio", 114, -1 ),
        new Among ( "\u0117jo", 113, -1 ),
        new Among ( "uo", 113, -1 ),
        new Among ( "s", -1, -1 ),
        new Among ( "as", 118, -1 ),
        new Among ( "ias", 119, -1 ),
        new Among ( "es", 118, -1 ),
        new Among ( "ies", 121, -1 ),
        new Among ( "is", 118, -1 ),
        new Among ( "ais", 123, -1 ),
        new Among ( "iais", 124, -1 ),
        new Among ( "tumeis", 123, -1 ),
        new Among ( "imis", 123, -1 ),
        new Among ( "enimis", 127, -1 ),
        new Among ( "omis", 123, -1 ),
        new Among ( "iomis", 129, -1 ),
        new Among ( "umis", 123, -1 ),
        new Among ( "\u0117mis", 123, -1 ),
        new Among ( "enis", 123, -1 ),
        new Among ( "asis", 123, -1 ),
        new Among ( "ysis", 123, -1 ),
        new Among ( "ams", 118, -1 ),
        new Among ( "iams", 136, -1 ),
        new Among ( "iems", 118, -1 ),
        new Among ( "ims", 118, -1 ),
        new Among ( "enims", 139, -1 ),
        new Among ( "erims", 139, -1 ),
        new Among ( "oms", 118, -1 ),
        new Among ( "ioms", 142, -1 ),
        new Among ( "ums", 118, -1 ),
        new Among ( "\u0117ms", 118, -1 ),
        new Among ( "ens", 118, -1 ),
        new Among ( "os", 118, -1 ),
        new Among ( "ios", 147, -1 ),
        new Among ( "uos", 147, -1 ),
        new Among ( "iuos", 149, -1 ),
        new Among ( "ers", 118, -1 ),
        new Among ( "us", 118, -1 ),
        new Among ( "aus", 152, -1 ),
        new Among ( "iaus", 153, -1 ),
        new Among ( "ius", 152, -1 ),
        new Among ( "ys", 118, -1 ),
        new Among ( "enys", 156, -1 ),
        new Among ( "erys", 156, -1 ),
        new Among ( "om\u00C4\u0097s", 118, -1 ),
        new Among ( "ot\u00C4\u0097s", 118, -1 ),
        new Among ( "\u0105s", 118, -1 ),
        new Among ( "i\u0105s", 161, -1 ),
        new Among ( "\u0117s", 118, -1 ),
        new Among ( "am\u0117s", 163, -1 ),
        new Among ( "iam\u0117s", 164, -1 ),
        new Among ( "im\u0117s", 163, -1 ),
        new Among ( "kim\u0117s", 166, -1 ),
        new Among ( "sim\u0117s", 166, -1 ),
        new Among ( "om\u0117s", 163, -1 ),
        new Among ( "\u0117m\u0117s", 163, -1 ),
        new Among ( "tum\u0117m\u0117s", 170, -1 ),
        new Among ( "at\u0117s", 163, -1 ),
        new Among ( "iat\u0117s", 172, -1 ),
        new Among ( "sit\u0117s", 163, -1 ),
        new Among ( "ot\u0117s", 163, -1 ),
        new Among ( "\u0117t\u0117s", 163, -1 ),
        new Among ( "tum\u0117t\u0117s", 176, -1 ),
        new Among ( "\u012Fs", 118, -1 ),
        new Among ( "\u016Bs", 118, -1 ),
        new Among ( "t\u0173s", 118, -1 ),
        new Among ( "at", -1, -1 ),
        new Among ( "iat", 181, -1 ),
        new Among ( "it", -1, -1 ),
        new Among ( "sit", 183, -1 ),
        new Among ( "ot", -1, -1 ),
        new Among ( "\u0117t", -1, -1 ),
        new Among ( "tum\u0117t", 186, -1 ),
        new Among ( "u", -1, -1 ),
        new Among ( "au", 188, -1 ),
        new Among ( "iau", 189, -1 ),
        new Among ( "\u010Diau", 190, -1 ),
        new Among ( "iu", 188, -1 ),
        new Among ( "eniu", 192, -1 ),
        new Among ( "siu", 192, -1 ),
        new Among ( "y", -1, -1 ),
        new Among ( "\u0105", -1, -1 ),
        new Among ( "i\u0105", 196, -1 ),
        new Among ( "\u0117", -1, -1 ),
        new Among ( "\u0119", -1, -1 ),
        new Among ( "\u012F", -1, -1 ),
        new Among ( "en\u012F", 200, -1 ),
        new Among ( "er\u012F", 200, -1 ),
        new Among ( "\u0173", -1, -1 ),
        new Among ( "i\u0173", 203, -1 ),
        new Among ( "er\u0173", 203, -1 )
    };

    private final static Among a_1[] = {
        new Among ( "ing", -1, -1 ),
        new Among ( "aj", -1, -1 ),
        new Among ( "iaj", 1, -1 ),
        new Among ( "iej", -1, -1 ),
        new Among ( "oj", -1, -1 ),
        new Among ( "ioj", 4, -1 ),
        new Among ( "uoj", 4, -1 ),
        new Among ( "iuoj", 6, -1 ),
        new Among ( "auj", -1, -1 ),
        new Among ( "\u0105j", -1, -1 ),
        new Among ( "i\u0105j", 9, -1 ),
        new Among ( "\u0117j", -1, -1 ),
        new Among ( "\u0173j", -1, -1 ),
        new Among ( "i\u0173j", 12, -1 ),
        new Among ( "ok", -1, -1 ),
        new Among ( "iok", 14, -1 ),
        new Among ( "iuk", -1, -1 ),
        new Among ( "uliuk", 16, -1 ),
        new Among ( "u\u010Diuk", 16, -1 ),
        new Among ( "i\u0161k", -1, -1 ),
        new Among ( "iul", -1, -1 ),
        new Among ( "yl", -1, -1 ),
        new Among ( "\u0117l", -1, -1 ),
        new Among ( "am", -1, -1 ),
        new Among ( "dam", 23, -1 ),
        new Among ( "jam", 23, -1 ),
        new Among ( "zgan", -1, -1 ),
        new Among ( "ain", -1, -1 ),
        new Among ( "esn", -1, -1 ),
        new Among ( "op", -1, -1 ),
        new Among ( "iop", 29, -1 ),
        new Among ( "ias", -1, -1 ),
        new Among ( "ies", -1, -1 ),
        new Among ( "ais", -1, -1 ),
        new Among ( "iais", 33, -1 ),
        new Among ( "os", -1, -1 ),
        new Among ( "ios", 35, -1 ),
        new Among ( "uos", 35, -1 ),
        new Among ( "iuos", 37, -1 ),
        new Among ( "aus", -1, -1 ),
        new Among ( "iaus", 39, -1 ),
        new Among ( "\u0105s", -1, -1 ),
        new Among ( "i\u0105s", 41, -1 ),
        new Among ( "\u0119s", -1, -1 ),
        new Among ( "ut\u0117ait", -1, -1 ),
        new Among ( "ant", -1, -1 ),
        new Among ( "iant", 45, -1 ),
        new Among ( "siant", 46, -1 ),
        new Among ( "int", -1, -1 ),
        new Among ( "ot", -1, -1 ),
        new Among ( "uot", 49, -1 ),
        new Among ( "iuot", 50, -1 ),
        new Among ( "yt", -1, -1 ),
        new Among ( "\u0117t", -1, -1 ),
        new Among ( "yk\u0161t", -1, -1 ),
        new Among ( "iau", -1, -1 ),
        new Among ( "dav", -1, -1 ),
        new Among ( "sv", -1, -1 ),
        new Among ( "\u0161v", -1, -1 ),
        new Among ( "yk\u0161\u010D", -1, -1 ),
        new Among ( "\u0119", -1, -1 ),
        new Among ( "\u0117j\u0119", 60, -1 )
    };

    private final static Among a_2[] = {
        new Among ( "ojime", -1, 9 ),
        new Among ( "\u0117jime", -1, 5 ),
        new Among ( "avime", -1, 8 ),
        new Among ( "okate", -1, 11 ),
        new Among ( "aite", -1, 1 ),
        new Among ( "uote", -1, 4 ),
        new Among ( "asius", -1, 7 ),
        new Among ( "okat\u0117s", -1, 10 ),
        new Among ( "ait\u0117s", -1, 2 ),
        new Among ( "uot\u0117s", -1, 3 ),
        new Among ( "esiu", -1, 6 )
    };

    private final static Among a_3[] = {
        new Among ( "\u010D", -1, 1 ),
        new Among ( "d\u017E", -1, 2 )
    };

    private final static Among a_4[] = {
        new Among ( "gd", -1, 1 )
    };

    private static final char g_v[] = {17, 65, 16, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 0, 64, 1, 0, 64, 0, 0, 0, 0, 0, 0, 0, 4, 4 };

    private boolean B_CHANGE;
    private int I_s;
    private int I_p2;
    private int I_p1;


    private boolean r_R1() {
        if (!(I_p1 <= cursor))
        {
            return false;
        }
        return true;
    }

    private boolean r_step1() {
        int v_1;
        int v_2;
        // (, line 48
        // setlimit, line 49
        v_1 = limit - cursor;
        // tomark, line 49
        if (cursor < I_p1)
        {
            return false;
        }
        cursor = I_p1;
        v_2 = limit_backward;
        limit_backward = cursor;
        cursor = limit - v_1;
        // (, line 49
        // [, line 49
        ket = cursor;
        // substring, line 49
        if (find_among_b(a_0) == 0)
        {
            limit_backward = v_2;
            return false;
        }
        // ], line 49
        bra = cursor;
        limit_backward = v_2;
        // call R1, line 49
        if (!r_R1())
        {
            return false;
        }
        // delete, line 233
        slice_del();
        return true;
    }

    private boolean r_step2() {
        int v_1;
        int v_2;
        int v_3;
        // repeat, line 236
        replab0: while(true)
        {
            v_1 = limit - cursor;
            lab1: do {
                // (, line 236
                // setlimit, line 237
                v_2 = limit - cursor;
                // tomark, line 237
                if (cursor < I_p1)
                {
                    break lab1;
                }
                cursor = I_p1;
                v_3 = limit_backward;
                limit_backward = cursor;
                cursor = limit - v_2;
                // (, line 237
                // [, line 237
                ket = cursor;
                // substring, line 237
                if (find_among_b(a_1) == 0)
                {
                    limit_backward = v_3;
                    break lab1;
                }
                // ], line 237
                bra = cursor;
                limit_backward = v_3;
                // delete, line 307
                slice_del();
                continue replab0;
            } while (false);
            cursor = limit - v_1;
            break replab0;
        }
        return true;
    }

    private boolean r_fix_conflicts() {
        int among_var;
        // (, line 310
        // [, line 311
        ket = cursor;
        // substring, line 311
        among_var = find_among_b(a_2);
        if (among_var == 0)
        {
            return false;
        }
        // ], line 311
        bra = cursor;
        switch (among_var) {
            case 0:
                return false;
            case 1:
                // (, line 313
                // <-, line 313
                slice_from("ait\u0117");
                // set CHANGE, line 313
                B_CHANGE = true;
                break;
            case 2:
                // (, line 315
                // <-, line 315
                slice_from("ait\u0117");
                // set CHANGE, line 315
                B_CHANGE = true;
                break;
            case 3:
                // (, line 318
                // <-, line 318
                slice_from("uot\u0117");
                // set CHANGE, line 318
                B_CHANGE = true;
                break;
            case 4:
                // (, line 320
                // <-, line 320
                slice_from("uot\u0117");
                // set CHANGE, line 320
                B_CHANGE = true;
                break;
            case 5:
                // (, line 323
                // <-, line 323
                slice_from("\u0117jimas");
                // set CHANGE, line 323
                B_CHANGE = true;
                break;
            case 6:
                // (, line 326
                // <-, line 326
                slice_from("esys");
                // set CHANGE, line 326
                B_CHANGE = true;
                break;
            case 7:
                // (, line 328
                // <-, line 328
                slice_from("asys");
                // set CHANGE, line 328
                B_CHANGE = true;
                break;
            case 8:
                // (, line 331
                // <-, line 331
                slice_from("avimas");
                // set CHANGE, line 331
                B_CHANGE = true;
                break;
            case 9:
                // (, line 332
                // <-, line 332
                slice_from("ojimas");
                // set CHANGE, line 332
                B_CHANGE = true;
                break;
            case 10:
                // (, line 335
                // <-, line 335
                slice_from("okat\u0117");
                // set CHANGE, line 335
                B_CHANGE = true;
                break;
            case 11:
                // (, line 337
                // <-, line 337
                slice_from("okat\u0117");
                // set CHANGE, line 337
                B_CHANGE = true;
                break;
        }
        return true;
    }

    private boolean r_fix_chdz() {
        int among_var;
        // (, line 341
        // [, line 342
        ket = cursor;
        // substring, line 342
        among_var = find_among_b(a_3);
        if (among_var == 0)
        {
            return false;
        }
        // ], line 342
        bra = cursor;
        switch (among_var) {
            case 0:
                return false;
            case 1:
                // (, line 343
                // <-, line 343
                slice_from("t");
                // set CHANGE, line 343
                B_CHANGE = true;
                break;
            case 2:
                // (, line 344
                // <-, line 344
                slice_from("d");
                // set CHANGE, line 344
                B_CHANGE = true;
                break;
        }
        return true;
    }

    private boolean r_fix_gd() {
        int among_var;
        // (, line 348
        // [, line 349
        ket = cursor;
        // substring, line 349
        among_var = find_among_b(a_4);
        if (among_var == 0)
        {
            return false;
        }
        // ], line 349
        bra = cursor;
        switch (among_var) {
            case 0:
                return false;
            case 1:
                // (, line 350
                // <-, line 350
                slice_from("g");
                // set CHANGE, line 350
                B_CHANGE = true;
                break;
        }
        return true;
    }

    public boolean stem() {
        int v_1;
        int v_2;
        int v_3;
        int v_8;
        int v_9;
        int v_10;
        int v_11;
        int v_12;
        int v_13;
        // (, line 357
        I_p1 = limit;
        I_p2 = limit;
        I_s = (current.length());
        // do, line 363
        v_1 = cursor;
        lab0: do {
            // (, line 363
            // try, line 365
            v_2 = cursor;
            lab1: do {
                // (, line 365
                // test, line 365
                v_3 = cursor;
                // literal, line 365
                if (!(eq_s("a")))
                {
                    cursor = v_2;
                    break lab1;
                }
                cursor = v_3;
                if (!(I_s > 6))
                {
                    cursor = v_2;
                    break lab1;
                }
                // hop, line 365
                {
                    int c = cursor + 1;
                    if (0 > c || c > limit)
                    {
                        cursor = v_2;
                        break lab1;
                    }
                    cursor = c;
                }
            } while (false);
            // gopast, line 367
            golab2: while(true)
            {
                lab3: do {
                    if (!(in_grouping(g_v, 97, 371)))
                    {
                        break lab3;
                    }
                    break golab2;
                } while (false);
                if (cursor >= limit)
                {
                    break lab0;
                }
                cursor++;
            }
            // gopast, line 367
            golab4: while(true)
            {
                lab5: do {
                    if (!(out_grouping(g_v, 97, 371)))
                    {
                        break lab5;
                    }
                    break golab4;
                } while (false);
                if (cursor >= limit)
                {
                    break lab0;
                }
                cursor++;
            }
            // setmark p1, line 367
            I_p1 = cursor;
            // gopast, line 368
            golab6: while(true)
            {
                lab7: do {
                    if (!(in_grouping(g_v, 97, 371)))
                    {
                        break lab7;
                    }
                    break golab6;
                } while (false);
                if (cursor >= limit)
                {
                    break lab0;
                }
                cursor++;
            }
            // gopast, line 368
            golab8: while(true)
            {
                lab9: do {
                    if (!(out_grouping(g_v, 97, 371)))
                    {
                        break lab9;
                    }
                    break golab8;
                } while (false);
                if (cursor >= limit)
                {
                    break lab0;
                }
                cursor++;
            }
            // setmark p2, line 368
            I_p2 = cursor;
        } while (false);
        cursor = v_1;
        // backwards, line 371
        limit_backward = cursor; cursor = limit;
        // (, line 371
        // do, line 372
        v_8 = limit - cursor;
        lab10: do {
            // call fix_conflicts, line 372
            if (!r_fix_conflicts())
            {
                break lab10;
            }
        } while (false);
        cursor = limit - v_8;
        // do, line 373
        v_9 = limit - cursor;
        lab11: do {
            // call step1, line 373
            if (!r_step1())
            {
                break lab11;
            }
        } while (false);
        cursor = limit - v_9;
        // do, line 374
        v_10 = limit - cursor;
        lab12: do {
            // call fix_chdz, line 374
            if (!r_fix_chdz())
            {
                break lab12;
            }
        } while (false);
        cursor = limit - v_10;
        // do, line 375
        v_11 = limit - cursor;
        lab13: do {
            // call step2, line 375
            if (!r_step2())
            {
                break lab13;
            }
        } while (false);
        cursor = limit - v_11;
        // do, line 376
        v_12 = limit - cursor;
        lab14: do {
            // call fix_chdz, line 376
            if (!r_fix_chdz())
            {
                break lab14;
            }
        } while (false);
        cursor = limit - v_12;
        // do, line 377
        v_13 = limit - cursor;
        lab15: do {
            // call fix_gd, line 377
            if (!r_fix_gd())
            {
                break lab15;
            }
        } while (false);
        cursor = limit - v_13;
        cursor = limit_backward;        return true;
    }

    public boolean equals( Object o ) {
        return o instanceof LithuanianStemmer;
    }

    public int hashCode() {
        return LithuanianStemmer.class.getName().hashCode();
    }



}

