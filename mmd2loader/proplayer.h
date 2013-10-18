/* © Fully Copyrighted SEE ReadMe FILE */
/* C definitions for using the 'proplayer.a' and 'pro8player.a'
   play-routines of OctaMED Pro V3 - V5. OctaMED Pro V5 -specific
   features are marked as '(V5)'. */

/* $VER: proplayer_h 5.000 (30.05.1993) */

#ifndef EXEC_TYPES_H
#include <exec/types.h>
#endif

#ifdef LATTICE
#ifndef OCTAPLR_LIB_PROTOS
/* In 'proplayer.a' */
LONG __asm InitPlayer(void);
void __asm RemPlayer(void);
void __asm PlayModule(register __a0 struct MMD0 *);
void __asm ContModule(register __a0 struct MMD0 *);
void __asm StopPlayer(void);
void __asm SetTempo(register __d0 UWORD);
/* In 'proloadmod.a' */
struct MMD0 * __asm LoadModule(register __a0 char *);
void __asm RelocModule(register __a0 struct MMD0 *);
void __asm UnLoadModule(register __a0 struct MMD0 *);
/* These are the definitions for the 8-channel OctaMED routines,
   in 'pro8player.a' */
LONG __asm InitPlayer8(void);
void __asm RemPlayer8(void);
void __asm PlayModule8(register __a0 struct MMD0 *);
void __asm ContModule8(register __a0 struct MMD0 *);
void __asm StopPlayer8(void);
#endif
#endif

/* If you're playing multi-modules, set the 'modnum' variable to the
   number of the song you want to play before calling PlayModule(). */

#ifdef LATTICE_50
extern UWORD far modnum;

/* 'modnum8' is the equivalent in 'mod8player' */

extern UWORD far modnum8;
#else
extern UWORD modnum,modnum8; /* for less intelligent compilers */
#endif

/* This is the main module structure */
struct MMD0 {   /* Also for MMD1 and MMD2 */
    ULONG   id;         /* "MMD0", "MMD1" or "MMD2" */
    ULONG   modlen;         /* module length (in bytes) */
    struct  MMD0song *song;     /* pointer to MMD0song */
    UWORD   psecnum;        /* (MMD2) - used by the player */
    UWORD   pseq;           /* (MMD2) - used by the player */
    struct  MMD0block **blockarr;   /* pointer to pointers of blocks */
    ULONG   reserved1;
    struct  Soitin **smplarr;   /* pointer to pointers of samples */
    ULONG   reserved2;
    struct  MMD0exp *expdata;   /* pointer to expansion data */
    ULONG   reserved3;
/* The following values are used by the play routine */
    UWORD   pstate;         /* the state of the player */
    UWORD   pblock;         /* current block */
    UWORD   pline;          /* current line */
    UWORD   pseqnum;        /* current # of playseqlist */
    WORD    actplayline;        /* OBSOLETE!! SET TO 0xFFFF! */
    UBYTE   counter;        /* delay between notes */
    UBYTE   extra_songs;        /* number of additional songs, see
                       expdata->nextmod */
};

/* These are the structures for future expansions */

struct InstrExt {   /* This struct only for data required for playing */
/* NOTE: THIS STRUCTURE MAY GROW IN THE FUTURE, TO GET THE CORRECT SIZE,
   EXAMINE mmd0->expdata->s_ext_entrsz */
/* ALSO NOTE: THIS STRUCTURE MAY BE SHORTER THAN DESCRIBED HERE,
   EXAMINE mmd0->expdata->s_ext_entrsz */
    UBYTE hold;
    UBYTE decay;
    UBYTE suppress_midi_off;    /* 1 = suppress, 0 = don't */
    BYTE  finetune;
    UBYTE default_pitch;    /* (V5) */
    UBYTE instr_flags;  /* (V5) */
    UWORD long_midi_preset; /* (V5), overrides the preset in the
        song structure, if this exists, MMD0sample/midipreset
        should not be used. */
};

struct MMDInstrInfo {
    UBYTE   name[40];
    UBYTE   pad0;   /* two pads? */
    UBYTE   pad1;
};

struct MMD0exp {
    struct MMD0 *nextmod;       /* for multi-modules */
    struct InstrExt *exp_smp;   /* pointer to an array of InstrExts */
    UWORD  s_ext_entries;       /* # of InstrExts in the array */
    UWORD  s_ext_entrsz;        /* size of an InstrExt structure */
    UBYTE  *annotxt;        /* 0-terminated message string */
    ULONG  annolen;         /* length (including the 0-byte) */
/* MED V3.20 data below... */
    struct MMDInstrInfo *iinfo; /* "secondary" InstrExt for info
                       that does not affect output */
    UWORD  i_ext_entries;       /* # of MMDInstrInfos */
    UWORD  i_ext_entrsz;        /* size of one */
    ULONG  jumpmask;        /* OBSOLETE in current OctaMEDs */
    UWORD  *rgbtable;       /* pointer to 8 UWORD values,
                       ignored by OctaMED V5 and later */
    UBYTE  channelsplit[4]; /* for OctaMED only (non-zero = NOT splitted) */
    struct NotationInfo *n_info;    /* OctaMED notation editor info data */
    UBYTE  *songname;   /* song name */
    ULONG  songnamelen; /* length (including terminating zero) */
    struct MMDDumpData *dumps; /* MIDI message dump data */
/* These are still left, they must be 0 at the moment. */
    ULONG  reserved2[7];
};

/* Info for each instrument (mmd0->song.sample[xx]) */

struct MMD0sample {
    UWORD rep,replen;   /* repeat/repeat length */
    UBYTE midich;       /* midi channel for curr. instrument */
    UBYTE midipreset;   /* midi preset (1 - 128), 0 = no preset */
    UBYTE svol;     /* default volume */
    BYTE strans;        /* sample transpose */
};

/* The song structure (mmd0->song) */

struct MMD0song {
    struct MMD0sample sample[63];   /* info for each instrument */
    UWORD   numblocks;      /* number of blocks in this song */
    UWORD   songlen;        /* number of playseq entries */
    UBYTE   playseq[256];       /* the playseq list */
    UWORD   deftempo;       /* default tempo */
    BYTE    playtransp;     /* play transpose */
    UBYTE   flags;          /* flags (see below) */
    UBYTE   reserved;       /* for future expansion */
    UBYTE   tempo2;         /* 2ndary tempo (delay betw. notes) */
    UBYTE   trkvol[16];     /* track volume */
    UBYTE   mastervol;      /* master volume */
    UBYTE   numsamples;     /* number of instruments */
}; /* length = 788 bytes */

/* The new PlaySeq structure of MMD2 */

struct PlaySeq {
    char    name[32];   /* (0)  31 chars + \0 */
    ULONG   reserved[2];    /* (32) for possible extensions */
    UWORD   length;     /* (40) # of entries */
/* Commented out, not all compilers may like it... */
/*  UWORD   seq[0]; */  /* (42) block numbers.. */
/* Note: seq[] values above 0x7FFF are reserved for future expansion! */
};

/* This structure is used in MMD2s, instead of the above one.
   (Be sure to cast the pointer.) */

struct MMD2song {
    struct MMD0sample sample[63];
    UWORD   numblocks;
    UWORD   songlen;    /* NOTE: number of sections in MMD2 */
    struct  PlaySeq **playseqtable;
    UWORD   *sectiontable;  /* UWORD section numbers */
    UBYTE   *trackvols; /* UBYTE track volumes */
    UWORD   numtracks;  /* max. number of tracks in the song
                   (also the number of entries in
                    'trackvols' table) */
    UWORD   numpseqs;   /* number of PlaySeqs in 'playseqtable' */
    UBYTE   pad0[240];  /* reserved for future expansion */
/* Below fields are MMD0/MMD1-compatible (except pad1[]) */
    UWORD   deftempo;
    BYTE    playtransp;
    UBYTE   flags;
    UBYTE   flags2;
    UBYTE   tempo2;
    UBYTE   pad1[16];   /* used to be trackvols, in MMD2 reserved */
    UBYTE   mastervol;
    UBYTE   numsamples;
};

 /* FLAGS of the above structure */
#define FLAG_FILTERON   0x1 /* hardware low-pass filter */
#define FLAG_JUMPINGON  0x2 /* OBSOLETE now, but retained for compatibility */
#define FLAG_JUMP8TH    0x4 /* also OBSOLETE */
#define FLAG_INSTRSATT  0x8 /* instruments are attached (sng+samples)
                   used only in saved (old) MED-songs */
#define FLAG_VOLHEX 0x10    /* volumes are represented as hex */
#define FLAG_STSLIDE    0x20    /* no effects on 1st timing pulse (STS) */
#define FLAG_8CHANNEL   0x40    /* OctaMED 8 channel song, examine this bit
                   to find out which routine to use */
/* flags2 */
#define FLAG2_BMASK 0x1F
#define FLAG2_BPM   0x20

struct MMDDump {
    ULONG   length;     /* dump data length */
    UBYTE   *data;      /* data pointer */
    UWORD   ext_len;    /* bytes remaining in this struct */
/* ext_len >= 20: */
    UBYTE   name[20];   /* message name (null-terminated) */
};

struct MMDDumpData {
    UWORD   numdumps;   /* number of message dumps */
    UWORD   reserved[3];    /* not currently used */
};  // Followed by <numdumps> pointers to struct MMDDump

/* flags in struct NotationInfo */
#define NFLG_FLAT 1
#define NFLG_3_4  2

struct NotationInfo {
    UBYTE n_of_sharps;  /* number of #'s (or b's) */
    UBYTE flags;        /* flags (see above) */
    WORD  trksel[5];    /* selected track for each preset (-1 = none) */
    UBYTE trkshow[16];  /* which tracks to show (bit 0 = for preset 0,
                bit 1 for preset 1 and so on..) */
    UBYTE trkghost[16]; /* ghosted tracks (like trkshow[]) */
    BYTE  notetr[63];       /* -24 - +24 (if bit #6 is negated, hidden) */
    UBYTE pad;  /* perhaps info about future extensions */
};

/* Below structs for MMD1 only! */
struct BlockInfo {
    ULONG   *hlmask;    /* highlight data */
    UBYTE   *blockname; /* block name */
    ULONG   blocknamelen;   /* length of block name (including term. 0) */
    ULONG   reserved[6];    /* future expansion */
};

struct MMD1Block {
    UWORD numtracks;
    UWORD lines;
    struct BlockInfo *info;
};
#define MMD1BLKHDRSZ 8

/* Please refer to 'MMD.txt' for a complete description of MMD file format. */
