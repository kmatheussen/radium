
import sys,os,string



def GFX_MenuDialog(filename,*items):
  file=open(filename,'w')
  file.writelines(map(lambda x:x+"\n", items))
  file.close()
  os.system("python X11_MenuDialog.py "+filename);


if __name__=="__main__":    

  import qt,string
    
  class WidgetView ( qt.QWidget ):
    def __init__( self, *args ):
      apply( qt.QWidget.__init__, (self,) + args )

      self.topLayout = qt.QVBoxLayout( self, 10 )
      self.grid = qt.QGridLayout( 0, 0 )
      self.topLayout.addLayout( self.grid, 10 )

      # Create a list box
      self.lb = qt.QListBox( self, "listBox" )

      file=open(sys.argv[1],'r')
      self.dasitems=map(lambda x:string.rstrip(x),file.readlines())
      file.close()

      self.setCaption(self.dasitems.pop(0))
      
      for item in self.dasitems:
        self.lb.insertItem(item)

      self.grid.addMultiCellWidget( self.lb, 0, 0, 0, 0 )
      self.connect( self.lb, qt.SIGNAL("selected(int)"), self.listBoxItemSelected )

      self.topLayout.activate()
      
    def listBoxItemSelected( self, index ):
        txt = qt.QString()
        txt = "List box item %d selected" % index
        print txt
        file=open(sys.argv[1],'w')
        file.write(self.dasitems[index])
        file.close();
        a.quit()

  a = qt.QApplication( sys.argv )
  
  w = WidgetView()

  a.setMainWidget( w )
  w.show()
  a.exec_loop()

