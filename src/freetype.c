
#include "freetype.h"

SEXP glyphMetrics(SEXP font) {
    FT_Library  ft_library;
    FT_Face     face; 
    long        numGlyphs;
    int         i, glyphErr, err;

    SEXP unitsPerEm, glyphInfo, metrics, result;
    PROTECT(result = allocVector(VECSXP, 2));

    err = FT_Init_FreeType(&ft_library);
    if (err) {
        error("FreeType initialisation failed");
    }

    err = FT_New_Face(ft_library,
                      CHAR(STRING_ELT(font, 0)),
                      0,
                      &face);
    if (err == FT_Err_Unknown_File_Format) {
        error("Font read failed: Unknown font format");
    } else if (err) {
        error("Font read failed");
    } 

    err = FT_Set_Char_Size(face, 0, 12*64, 96, 0);
    if (err) {
        error("Set char size failed");
    }

    PROTECT(unitsPerEm = allocVector(INTSXP, 1));
    INTEGER(unitsPerEm)[0] = face->units_per_EM;
    SET_VECTOR_ELT(result, 0, unitsPerEm);
    UNPROTECT(1);

    numGlyphs = face->num_glyphs;
    PROTECT(metrics = allocVector(VECSXP, numGlyphs));
    glyphErr = 0;
    for (i=0; i < numGlyphs; i++) {
        PROTECT(glyphInfo = allocVector(INTSXP, 5));
        err = FT_Load_Glyph(face, i, FT_LOAD_NO_SCALE);
        if (err) {
            glyphErr = 1;
            INTEGER(glyphInfo)[0] = NA_INTEGER;
            INTEGER(glyphInfo)[1] = NA_INTEGER;
            INTEGER(glyphInfo)[2] = NA_INTEGER;
            INTEGER(glyphInfo)[3] = NA_INTEGER;
            INTEGER(glyphInfo)[4] = NA_INTEGER;
        } else {
            INTEGER(glyphInfo)[0] = face->glyph->metrics.horiAdvance;
            INTEGER(glyphInfo)[1] = face->glyph->metrics.horiBearingX;
            INTEGER(glyphInfo)[2] = face->glyph->metrics.horiBearingX + 
                face->glyph->metrics.width;
            INTEGER(glyphInfo)[3] = face->glyph->metrics.horiBearingY;
            INTEGER(glyphInfo)[4] = face->glyph->metrics.horiBearingY - 
                face->glyph->metrics.height;
        }
        SET_VECTOR_ELT(metrics, i, glyphInfo);
        UNPROTECT(1);
    }
    SET_VECTOR_ELT(result, 1, metrics);
    UNPROTECT(1);

    if (glyphErr) {
        warning("One or more glyph loads failed;  there may be missing values");
    }

    err = FT_Done_Face(face);
    if (err) {
        error("Face clean up failed");
    }

    err = FT_Done_FreeType(ft_library);
    if (err) {
        error("FreeType shut down failed");
    }

    UNPROTECT(1);
    return result;
}

SEXP glyphIndex(SEXP code, SEXP font) {
    FT_Library ft_library;
    FT_Face face; 
    int index, err;
    SEXP result = R_NilValue;

    PROTECT(result = allocVector(INTSXP, 1));

    err = FT_Init_FreeType(&ft_library);
    if (err) {
        error("FreeType initialisation failed");
    } 

    err = FT_New_Face(ft_library,
                      CHAR(STRING_ELT(font, 0)),
                      0,
                      &face);
    if (err == FT_Err_Unknown_File_Format) {
        error("Font read failed: Unknown font format");
    } else if (err) {
        error("Font read failed");
    } 

    err = FT_Select_Charmap(face, FT_ENCODING_UNICODE);
    if (err) {
        error("Failed to select UNICODE charmap");
    } 

    index = FT_Get_Char_Index(face, INTEGER(code)[0]);
    if (!index) {
        error("Undefined character code");
    } 
    INTEGER(result)[0] = index;

    err = FT_Done_Face(face);
    if (err) {
        error("Face clean up failed");
    }

    err = FT_Done_FreeType(ft_library);
    if (err) {
        error("FreeType shut down failed");
    }

    UNPROTECT(1);

    return result;
}

