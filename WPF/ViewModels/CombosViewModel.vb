Public Class CombosViewModel

    Sub New()
        'Fluids.Add()

    End Sub
    Public Property Fluids() As New Dictionary(Of Integer, String) From {
                {0, "Oil and Water"},
                {1, "Dry and Wet Gas"},
                {2, "Retrograde Condensate"}
    }
    Public Property OutputRes() As New Dictionary(Of Integer, String) From {
                {0, "Show Calculating Data"},
                {1, "Hide Calculating Data"}
    }
    Public Property PvtModels() As New Dictionary(Of Integer, String) From {
               {0, "Black Oil"},
               {1, "Equation of State"}
    }

    Public Property Completions() As New Dictionary(Of Integer, String) From {
               {0, "Cased Hole"},
               {1, "Open hole"}
    }
    Public Property Separators() As New Dictionary(Of Integer, String) From {
              {0, "Single Stage Separator"},
              {1, "Two Stage Separator"}
    }
    Public Property GravelPacks() As New Dictionary(Of Integer, String) From {
              {0, "None"},
              {1, "Gravel Pack"},
              {2, "Pre Packed Screen"},
              {3, "Wire Wrapped Screen"},
              {4, "Slotted Liner"}
    }
    Public Property Emulsions() As New Dictionary(Of Integer, String) From {
              {0, "No"},
              {1, "Emulsion + Pump Viscosity Correction"}}
    Public Property InflowTypes() As New Dictionary(Of Integer, String) From {
             {0, "Single Branch"},
             {1, "MultiLateral Well"}}
    Public Property Hydrates() As New Dictionary(Of Integer, String) From {
             {0, "Disabled Warning"},
             {1, "Enabled Warning"}}
    Public Property GasConings() As New Dictionary(Of Integer, String) From {
             {0, "No"},
             {1, "Yes"}
    }
    Public Property WatVis() As New Dictionary(Of Integer, String) From {
             {0, "Use Default Correlation"},
             {1, "Use Pressure Corrected Correlation"}
    }

    Public Property VisMods() As New Dictionary(Of Integer, String) From {
             {0, "Newtonian Fluid"},
             {1, "Non Newtonian Fluid"}
    }
    Public Property IprMethods() As New Dictionary(Of Integer, String) From {
             {0, "PI Entry"},
             {1, "Vogel"},
             {2, "Composite"},
             {3, "Darcy"},
             {4, "Fetkovich"},
             {5, "MultiRate Fetkovich"},
             {6, "Jones"},
             {7, "MultiRate Jones"},
             {8, "Transient"},
             {9, "Hidraulically Fractured Well"},
             {10, "Horizontal Well- No Flow Boundaries"}
    }
    'Dim list As New Dictionary(Of Integer, String) From {
    '            {0, "Oil and Water"},
    '            {1, "Dry and Wet Gas"},
    '            {2, "Retrograde Condensate"}
    '        }



    '        Return list







End Class
