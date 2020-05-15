
extern LANGSPEC bool Save_Initialize(const filepath_t filename, const char *type);
extern LANGSPEC void Save_Clean(const filepath_t filename,struct Root *theroot, bool is_backup); // Sets dc.success to false if it fails.
extern LANGSPEC bool Save(struct Root *theroot);
extern LANGSPEC bool SaveAs2(const filepath_t filename, struct Root *theroot);
extern LANGSPEC bool SaveAs(struct Root *theroot); // Opens file requester, and then calls SaveAs2.
extern LANGSPEC bool SaveWithEmbeddedSamples(filepath_t filename, struct Root *theroot);
extern LANGSPEC bool SaveWithoutEmbeddedSamples(filepath_t filename, struct Root *theroot);
extern LANGSPEC bool Export_Song(const filepath_t filename, struct Root *theroot);
extern LANGSPEC void Save_Backup(const filepath_t filename, struct Root *theroot);


