# Copybooks

## Formas aceitas

```text
01 AREA.
05 CAMPO PIC X(10).
05 CAMPO
         PIC S9(9) COMP-5 VALUE 0.
07        PIC X(01).
-INC BOOK001
=INC BOOK002
```

## Fixed Format

```text
000120        05 CAMPO
^^^^^^        ^^^^^^^^^
prefixo       data item

000130                                 PIC X(10).
^^^^^^                                 ^^^^^^^^^^
prefixo                                clausula

000140*-----------------------------------------------------------------
^^^^^^******************************************************************
prefixo comentario

     -             DEPENDING ON QT-ITENS.
     ^
continuacao
```

## COPY Dialetal

```text
-INC BOOK001
=INC BOOK002
^
coluna 1 obrigatoria
```

## Fragmentos

```text
EXEC SQL DECLARE DB2SGT.TAB TABLE
( COL CHAR(12) NOT NULL )
END-EXEC.

999000-TRATA-ERRO SECTION.
    EXEC CICS
         HANDLE ABEND CANCEL
    END-EXEC
```

## Validacao

```sh
XDG_CACHE_HOME=/tmp/tree-sitter-cache \
tree-sitter parse --quiet --stat /home/matheus/dev/mainframe/data/sgt/copybook/*.cpy
```

Ultima amostra local:

```text
885 copybooks
869 sem erro
16 com erro
98.19% sucesso
```
