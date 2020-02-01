
extern LANGSPEC bool Save_Initialize(const filepath_t filename, const char *type);
extern LANGSPEC void Save_Clean(const filepath_t filename,struct Root *theroot, bool is_backup);
extern LANGSPEC void Save(struct Root *theroot);
extern LANGSPEC void SaveAs(struct Root *theroot);
extern LANGSPEC void SaveWithEmbeddedSamples(struct Root *theroot);
extern LANGSPEC void SaveWithoutEmbeddedSamples(struct Root *theroot);
extern LANGSPEC void Save_Backup(const filepath_t filename, struct Root *theroot);


