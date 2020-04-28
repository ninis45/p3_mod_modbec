using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ModeloCI;


namespace MsDos
{
    class Program
    {
        
        static void Main(string[] args)
        {
            Entities_ModeloCI db = new Entities_ModeloCI();
            ModeloProsper.Modelo modelo = new ModeloProsper.Modelo("A392620C-AA17-462E-A10E-3D16CFA526F9");


           // modelo.Execute("A392620C-AA17-462E-A10E-3D16CFA526F9");

           // var mod_pozo = db.VW_MOD_POZO.Where(w => w.IDPOZO == "892F201D-41F7-42BE-A7B0-A7643AE77A98").SingleOrDefault();
           // var scheduled = db.CabeceraPozoGBN.Where(w => w.idPozo == "892F201D-41F7-42BE-A7B0-A7643AE77A98").ToList()[0];

           // ///var Names = scheduled.nombreArchivo.Split("\" +ToCharArray());
           // // string NameMaster As String = Names(Names.Length - 1)

           // string PathFile = "C:/PVTs/Tmps/prueba.OUT";

           // //Dim Bytes() As Byte = System.Convert.FromBase64String(Archivo.archivo)

           // File.WriteAllBytes(PathFile, scheduled.archivo);

           //// var result = modelo.Execute(mod_pozo.IDMODPOZO,)
        }
    }
}
