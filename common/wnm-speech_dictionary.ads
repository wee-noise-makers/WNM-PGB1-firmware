with LPC_Synth;
with WNM.Speech;
with LPC_Synth.Vocab_Festival.MA;
with LPC_Synth.Vocab_Festival.DE;
with LPC_Synth.Vocab_Festival.AW;
with LPC_Synth.Vocab_Festival.AC;
with LPC_Synth.Vocab_Festival.IT;
with LPC_Synth.Vocab_Festival.CR;
with LPC_Synth.Vocab_Festival.NO;
with LPC_Synth.Vocab_Festival.EI;
with LPC_Synth.Vocab_Festival.WA;
with LPC_Synth.Vocab_Festival.HE;
with LPC_Synth.Vocab_Festival.BL;
with LPC_Synth.Vocab_Festival.GI;
with LPC_Synth.Vocab_Festival.YO;
with LPC_Synth.Vocab_Festival.OU;
with LPC_Synth.Vocab_Festival.SO;
with LPC_Synth.Vocab_Festival.WI;
with LPC_Synth.Vocab_Festival.BI;
with LPC_Synth.Vocab_Festival.CA;
with LPC_Synth.Vocab_Festival.TU;
with LPC_Synth.Vocab_Festival.AB;
with LPC_Synth.Vocab_Festival.AD;
with LPC_Synth.Vocab_Festival.BA;
with LPC_Synth.Vocab_Festival.SE;
with LPC_Synth.Vocab_Festival.HO;
with LPC_Synth.Vocab_Festival.AT_K;
with LPC_Synth.Vocab_Festival.DO_K;
with LPC_Synth.Vocab_Festival.AM;
with LPC_Synth.Vocab_Festival.RO;
with LPC_Synth.Vocab_Festival.WO;
with LPC_Synth.Vocab_Festival.AG;
with LPC_Synth.Vocab_Festival.GA;
with LPC_Synth.Vocab_Festival.UP;
with LPC_Synth.Vocab_Festival.ME;
with LPC_Synth.Vocab_Festival.EV;
with LPC_Synth.Vocab_Festival.AE;
with LPC_Synth.Vocab_Festival.YE;
with LPC_Synth.Vocab_Festival.LI;
with LPC_Synth.Vocab_Festival.NI;
with LPC_Synth.Vocab_Festival.CI;
with LPC_Synth.Vocab_Festival.KE;
with LPC_Synth.Vocab_Festival.MY;
with LPC_Synth.Vocab_Festival.GR;
with LPC_Synth.Vocab_Festival.AL;
with LPC_Synth.Vocab_Festival.SH;
with LPC_Synth.Vocab_Festival.ZE;
with LPC_Synth.Vocab_Festival.AN;
with LPC_Synth.Vocab_Festival.PO;
with LPC_Synth.Vocab_Festival.MO;
with LPC_Synth.Vocab_Festival.UN;
with LPC_Synth.Vocab_Festival.GE;
with LPC_Synth.Vocab_Festival.LO;
with LPC_Synth.Vocab_Festival.TR;
with LPC_Synth.Vocab_Festival.SI;
with LPC_Synth.Vocab_Festival.WH;
with LPC_Synth.Vocab_Festival.SA;
with LPC_Synth.Vocab_Festival.WE;
with LPC_Synth.Vocab_Festival.LE;
with LPC_Synth.Vocab_Festival.FO;
with LPC_Synth.Vocab_Festival.TA;
with LPC_Synth.Vocab_Festival.DA;
with LPC_Synth.Vocab_Festival.MU;
with LPC_Synth.Vocab_Festival.II;
with LPC_Synth.Vocab_Festival.KI;
with LPC_Synth.Vocab_Festival.ST;
with LPC_Synth.Vocab_Festival.BU;
with LPC_Synth.Vocab_Festival.HA;
with LPC_Synth.Vocab_Festival.AR;
with LPC_Synth.Vocab_Festival.FI;
with LPC_Synth.Vocab_Festival.IF_K;
with LPC_Synth.Vocab_Festival.VO;
with LPC_Synth.Vocab_Festival.CH;
with LPC_Synth.Vocab_Festival.BO;
with LPC_Synth.Vocab_Festival.IS_K;
with LPC_Synth.Vocab_Festival.OV;
with LPC_Synth.Vocab_Festival.RE;
with LPC_Synth.Vocab_Festival.CO;
with LPC_Synth.Vocab_Festival.GO;
with LPC_Synth.Vocab_Festival.TH;
with LPC_Synth.Vocab_Festival.BR;
with LPC_Synth.Vocab_Festival.UR;
with LPC_Synth.Vocab_Festival.OF_K;
with LPC_Synth.Vocab_Festival.TW;
with LPC_Synth.Vocab_Festival.ZO;
with LPC_Synth.Vocab_Festival.VI;
with LPC_Synth.Vocab_Festival.KN;
with LPC_Synth.Vocab_Festival.BY;
with LPC_Synth.Vocab_Festival.LA;
with LPC_Synth.Vocab_Festival.TE;
with LPC_Synth.Vocab_Festival.ON;
with LPC_Synth.Vocab_Festival.TI;
with LPC_Synth.Vocab_Festival.DI;
with LPC_Synth.Vocab_Festival.HU;
with LPC_Synth.Vocab_Festival.CE;
with LPC_Synth.Vocab_Festival.JU;
with LPC_Synth.Vocab_Festival.FR;
with LPC_Synth.Vocab_Festival.CL;
with LPC_Synth.Vocab_Festival.EY;
with LPC_Synth.Vocab_Festival.BE;
with LPC_Synth.Vocab_Festival.IN_K;
with LPC_Synth.Vocab_Festival.DR;
with LPC_Synth.Vocab_Festival.TO;

package WNM.Speech_Dictionary is
   package Vocab renames LPC_Synth.Vocab_Festival;
   --  286 words
   pragma Style_Checks (Off);
   Data : constant array (WNM.Speech.Word)
     of not null LPC_Synth.LPC_Data_Const_Acc
       := (
           Vocab.AB.About'Access ,
           Vocab.AB.Abstract_K'Access ,
           Vocab.AC.Accident'Access ,
           Vocab.AC.Acid'Access ,
           Vocab.AC.Acquired'Access ,
           Vocab.AC.Action'Access ,
           Vocab.AC.Activated'Access ,
           Vocab.AD.Advanced'Access ,
           Vocab.AE.Aerial'Access ,
           Vocab.AG.Again'Access ,
           Vocab.AG.Against'Access ,
           Vocab.AL.Alive'Access ,
           Vocab.AL.All_K'Access ,
           Vocab.AL.Allowed'Access ,
           Vocab.AL.Alone'Access ,
           Vocab.AL.Alpha'Access ,
           Vocab.AL.Altered'Access ,
           Vocab.AM.Amazing'Access ,
           Vocab.AN.Analyze'Access ,
           Vocab.AN.And_K'Access ,
           Vocab.AN.Angel'Access ,
           Vocab.AN.Angry'Access ,
           Vocab.AN.Animal'Access ,
           Vocab.AN.Answer'Access ,
           Vocab.AN.Anxiety'Access ,
           Vocab.AN.Any'Access ,
           Vocab.AN.Anymore'Access ,
           Vocab.AR.Arcade'Access ,
           Vocab.AR.Are'Access ,
           Vocab.AR.Arrest'Access ,
           Vocab.AR.Artificial'Access ,
           Vocab.AT_K.Attack'Access ,
           Vocab.AW.Away'Access ,
           Vocab.BA.Baby'Access ,
           Vocab.BA.Back'Access ,
           Vocab.BA.Bad'Access ,
           Vocab.BA.Based'Access ,
           Vocab.BA.Basic'Access ,
           Vocab.BA.Bass'Access ,
           Vocab.BA.Battery'Access ,
           Vocab.BE.Be'Access ,
           Vocab.BE.Beat'Access ,
           Vocab.BE.Beautiful'Access ,
           Vocab.BE.Beta'Access ,
           Vocab.BE.Better'Access ,
           Vocab.BI.Bike'Access ,
           Vocab.BI.Billion'Access ,
           Vocab.BI.Binary'Access ,
           Vocab.BI.Bird'Access ,
           Vocab.BI.Bite'Access ,
           Vocab.BL.Black'Access ,
           Vocab.BL.Blame'Access ,
           Vocab.BL.Block'Access ,
           Vocab.BL.Blue'Access ,
           Vocab.BO.Bodies'Access ,
           Vocab.BO.Body_K'Access ,
           Vocab.BO.Bored'Access ,
           Vocab.BO.Boss'Access ,
           Vocab.BO.Box'Access ,
           Vocab.BO.Boy'Access ,
           Vocab.BR.Brain'Access ,
           Vocab.BR.Brakes'Access ,
           Vocab.BR.Break'Access ,
           Vocab.BR.Broadcast'Access ,
           Vocab.BR.Broke'Access ,
           Vocab.BR.Broken'Access ,
           Vocab.BR.Brother'Access ,
           Vocab.BR.Brutal'Access ,
           Vocab.BU.Bubble'Access ,
           Vocab.BU.Buddy'Access ,
           Vocab.BU.Bugs'Access ,
           Vocab.BU.Bullet'Access ,
           Vocab.BU.Burn'Access ,
           Vocab.BY.Bye'Access ,
           Vocab.CA.Cable'Access ,
           Vocab.CA.Cake'Access ,
           Vocab.CA.Called'Access ,
           Vocab.CA.Calls'Access ,
           Vocab.CA.Can'Access ,
           Vocab.CA.Cancelled'Access ,
           Vocab.CA.Candy'Access ,
           Vocab.CA.Cant'Access ,
           Vocab.CA.Carbon'Access ,
           Vocab.CA.Cash'Access ,
           Vocab.CA.Catch'Access ,
           Vocab.CE.Celebrate'Access ,
           Vocab.CE.Celebration'Access ,
           Vocab.CE.Centuries'Access ,
           Vocab.CH.Champion'Access ,
           Vocab.CH.Chaos'Access ,
           Vocab.CH.Cheap'Access ,
           Vocab.CH.Check'Access ,
           Vocab.CH.Checked'Access ,
           Vocab.CI.Citizen'Access ,
           Vocab.CL.Clear'Access ,
           Vocab.CL.Click'Access ,
           Vocab.CL.Clone'Access ,
           Vocab.CL.Club'Access ,
           Vocab.CO.Combat'Access ,
           Vocab.CO.Come'Access ,
           Vocab.CO.Computer'Access ,
           Vocab.CO.Connect'Access ,
           Vocab.CO.Console'Access ,
           Vocab.CO.Continue'Access ,
           Vocab.CO.Control'Access ,
           Vocab.CO.Cool'Access ,
           Vocab.CR.Crash'Access ,
           Vocab.CR.Crew'Access ,
           Vocab.DA.Damage'Access ,
           Vocab.DA.Danger'Access ,
           Vocab.DA.Darkness'Access ,
           Vocab.DA.Data'Access ,
           Vocab.DA.Day'Access ,
           Vocab.DE.Defects'Access ,
           Vocab.DE.Delete'Access ,
           Vocab.DE.Denied'Access ,
           Vocab.DE.Device'Access ,
           Vocab.DI.Dirt'Access ,
           Vocab.DO_K.Do_K'Access ,
           Vocab.DO_K.Done'Access ,
           Vocab.DO_K.Dont'Access ,
           Vocab.DO_K.Doubt'Access ,
           Vocab.DO_K.Down'Access ,
           Vocab.DO_K.Download'Access ,
           Vocab.DR.Dragon'Access ,
           Vocab.EI.Eight'Access ,
           Vocab.EV.Everything'Access ,
           Vocab.EY.Eyes'Access ,
           Vocab.FI.Fire'Access ,
           Vocab.FI.First'Access ,
           Vocab.FI.Five'Access ,
           Vocab.FO.For_K'Access ,
           Vocab.FO.Four'Access ,
           Vocab.FR.Free'Access ,
           Vocab.FR.From'Access ,
           Vocab.GA.Gadgets'Access ,
           Vocab.GE.Get'Access ,
           Vocab.GI.Girl'Access ,
           Vocab.GI.Girls'Access ,
           Vocab.GI.Give'Access ,
           Vocab.GO.Go'Access ,
           Vocab.GO.Gonna'Access ,
           Vocab.GO.Good'Access ,
           Vocab.GO.Got'Access ,
           Vocab.GR.Green'Access ,
           Vocab.HA.Hands'Access ,
           Vocab.HA.Have'Access ,
           Vocab.HE.Heart'Access ,
           Vocab.HE.Hey'Access ,
           Vocab.HO.Hold'Access ,
           Vocab.HO.How'Access ,
           Vocab.HU.Human'Access ,
           Vocab.II.I'Access ,
           Vocab.IF_K.If_K'Access ,
           Vocab.IN_K.In_K'Access ,
           Vocab.IS_K.Is_K'Access ,
           Vocab.IT.It'Access ,
           Vocab.IT.Its'Access ,
           Vocab.JU.Just'Access ,
           Vocab.KE.Keep'Access ,
           Vocab.KI.Kiss'Access ,
           Vocab.KN.Know'Access ,
           Vocab.LA.Lady'Access ,
           Vocab.LA.Last'Access ,
           Vocab.LE.Lets'Access ,
           Vocab.LI.Life'Access ,
           Vocab.LI.Like'Access ,
           Vocab.LI.Little'Access ,
           Vocab.LI.Live'Access ,
           Vocab.LO.Love'Access ,
           Vocab.LO.Loves'Access ,
           Vocab.MA.Make'Access ,
           Vocab.MA.Maker'Access ,
           Vocab.MA.Man'Access ,
           Vocab.ME.Me'Access ,
           Vocab.MO.More'Access ,
           Vocab.MU.Music'Access ,
           Vocab.MY.My'Access ,
           Vocab.NI.Night'Access ,
           Vocab.NI.Nine'Access ,
           Vocab.NO.No'Access ,
           Vocab.NO.Noise'Access ,
           Vocab.NO.Now'Access ,
           Vocab.OF_K.Of_K'Access ,
           Vocab.ON.On'Access ,
           Vocab.ON.One'Access ,
           Vocab.OU.Out_K'Access ,
           Vocab.OV.Over'Access ,
           Vocab.PO.Point'Access ,
           Vocab.RE.Red'Access ,
           Vocab.RO.Robot'Access ,
           Vocab.RO.Rock'Access ,
           Vocab.RO.Roll'Access ,
           Vocab.SA.Say'Access ,
           Vocab.SE.Seven'Access ,
           Vocab.SH.Shake'Access ,
           Vocab.SI.Sister'Access ,
           Vocab.SI.Six'Access ,
           Vocab.SO.So'Access ,
           Vocab.SO.Song'Access ,
           Vocab.ST.Stop'Access ,
           Vocab.TA.Take'Access ,
           Vocab.TE.Television'Access ,
           Vocab.TE.Tell'Access ,
           Vocab.TE.Ten'Access ,
           Vocab.TE.Tension'Access ,
           Vocab.TE.Terminal'Access ,
           Vocab.TE.Terror'Access ,
           Vocab.TE.Test'Access ,
           Vocab.TH.That'Access ,
           Vocab.TH.The'Access ,
           Vocab.TH.Theme'Access ,
           Vocab.TH.Theory'Access ,
           Vocab.TH.There'Access ,
           Vocab.TH.This'Access ,
           Vocab.TH.Three'Access ,
           Vocab.TH.Throw'Access ,
           Vocab.TH.Thumbs'Access ,
           Vocab.TI.Ticket'Access ,
           Vocab.TI.Tiger'Access ,
           Vocab.TI.Time'Access ,
           Vocab.TI.Times'Access ,
           Vocab.TI.Tissue'Access ,
           Vocab.TO.To'Access ,
           Vocab.TO.Today'Access ,
           Vocab.TO.Together'Access ,
           Vocab.TO.Tomorrow'Access ,
           Vocab.TO.Tonight'Access ,
           Vocab.TO.Too'Access ,
           Vocab.TO.Touch'Access ,
           Vocab.TO.Toxic'Access ,
           Vocab.TR.Track'Access ,
           Vocab.TR.Trash'Access ,
           Vocab.TU.Turbo'Access ,
           Vocab.TU.Turn'Access ,
           Vocab.TW.Twist'Access ,
           Vocab.TW.Two'Access ,
           Vocab.UN.Undefined'Access ,
           Vocab.UN.Unique'Access ,
           Vocab.UN.United'Access ,
           Vocab.UP.Up'Access ,
           Vocab.UP.Update'Access ,
           Vocab.UP.Upload'Access ,
           Vocab.UR.Ur'Access ,
           Vocab.UR.Urban'Access ,
           Vocab.VI.Vice'Access ,
           Vocab.VI.Victory'Access ,
           Vocab.VI.Vintage'Access ,
           Vocab.VI.Virtual'Access ,
           Vocab.VO.Voice'Access ,
           Vocab.VO.Volume'Access ,
           Vocab.WA.Wait'Access ,
           Vocab.WA.Waiting'Access ,
           Vocab.WA.Wake'Access ,
           Vocab.WA.Wanna'Access ,
           Vocab.WA.Want'Access ,
           Vocab.WA.Wanted'Access ,
           Vocab.WA.Warning'Access ,
           Vocab.WA.Watch'Access ,
           Vocab.WA.Wave'Access ,
           Vocab.WA.Way'Access ,
           Vocab.WE.We'Access ,
           Vocab.WE.Week'Access ,
           Vocab.WE.Weekend'Access ,
           Vocab.WE.Weird'Access ,
           Vocab.WE.Welcome'Access ,
           Vocab.WH.What'Access ,
           Vocab.WH.When_K'Access ,
           Vocab.WH.White'Access ,
           Vocab.WI.Wicked'Access ,
           Vocab.WI.Will'Access ,
           Vocab.WI.Wind'Access ,
           Vocab.WI.Winner'Access ,
           Vocab.WI.Wish'Access ,
           Vocab.WI.With_K'Access ,
           Vocab.WI.Without'Access ,
           Vocab.WO.Woman'Access ,
           Vocab.WO.World'Access ,
           Vocab.YE.Yeah'Access ,
           Vocab.YE.Year'Access ,
           Vocab.YE.Yellow'Access ,
           Vocab.YE.Yes'Access ,
           Vocab.YO.You'Access ,
           Vocab.YO.Your'Access ,
           Vocab.ZE.Zero'Access ,
           Vocab.ZO.Zone'Access 
          );
   About_Str : aliased constant String := "About";
   Abstract_Str : aliased constant String := "Abstract";
   Accident_Str : aliased constant String := "Accident";
   Acid_Str : aliased constant String := "Acid";
   Acquired_Str : aliased constant String := "Acquired";
   Action_Str : aliased constant String := "Action";
   Activated_Str : aliased constant String := "Activated";
   Advanced_Str : aliased constant String := "Advanced";
   Aerial_Str : aliased constant String := "Aerial";
   Again_Str : aliased constant String := "Again";
   Against_Str : aliased constant String := "Against";
   Alive_Str : aliased constant String := "Alive";
   All_Str : aliased constant String := "All";
   Allowed_Str : aliased constant String := "Allowed";
   Alone_Str : aliased constant String := "Alone";
   Alpha_Str : aliased constant String := "Alpha";
   Altered_Str : aliased constant String := "Altered";
   Amazing_Str : aliased constant String := "Amazing";
   Analyze_Str : aliased constant String := "Analyze";
   And_Str : aliased constant String := "And";
   Angel_Str : aliased constant String := "Angel";
   Angry_Str : aliased constant String := "Angry";
   Animal_Str : aliased constant String := "Animal";
   Answer_Str : aliased constant String := "Answer";
   Anxiety_Str : aliased constant String := "Anxiety";
   Any_Str : aliased constant String := "Any";
   Anymore_Str : aliased constant String := "Anymore";
   Arcade_Str : aliased constant String := "Arcade";
   Are_Str : aliased constant String := "Are";
   Arrest_Str : aliased constant String := "Arrest";
   Artificial_Str : aliased constant String := "Artificial";
   Attack_Str : aliased constant String := "Attack";
   Away_Str : aliased constant String := "Away";
   Baby_Str : aliased constant String := "Baby";
   Back_Str : aliased constant String := "Back";
   Bad_Str : aliased constant String := "Bad";
   Based_Str : aliased constant String := "Based";
   Basic_Str : aliased constant String := "Basic";
   Bass_Str : aliased constant String := "Bass";
   Battery_Str : aliased constant String := "Battery";
   Be_Str : aliased constant String := "Be";
   Beat_Str : aliased constant String := "Beat";
   Beautiful_Str : aliased constant String := "Beautiful";
   Beta_Str : aliased constant String := "Beta";
   Better_Str : aliased constant String := "Better";
   Bike_Str : aliased constant String := "Bike";
   Billion_Str : aliased constant String := "Billion";
   Binary_Str : aliased constant String := "Binary";
   Bird_Str : aliased constant String := "Bird";
   Bite_Str : aliased constant String := "Bite";
   Black_Str : aliased constant String := "Black";
   Blame_Str : aliased constant String := "Blame";
   Block_Str : aliased constant String := "Block";
   Blue_Str : aliased constant String := "Blue";
   Bodies_Str : aliased constant String := "Bodies";
   Body_Str : aliased constant String := "Body";
   Bored_Str : aliased constant String := "Bored";
   Boss_Str : aliased constant String := "Boss";
   Box_Str : aliased constant String := "Box";
   Boy_Str : aliased constant String := "Boy";
   Brain_Str : aliased constant String := "Brain";
   Brakes_Str : aliased constant String := "Brakes";
   Break_Str : aliased constant String := "Break";
   Broadcast_Str : aliased constant String := "Broadcast";
   Broke_Str : aliased constant String := "Broke";
   Broken_Str : aliased constant String := "Broken";
   Brother_Str : aliased constant String := "Brother";
   Brutal_Str : aliased constant String := "Brutal";
   Bubble_Str : aliased constant String := "Bubble";
   Buddy_Str : aliased constant String := "Buddy";
   Bugs_Str : aliased constant String := "Bugs";
   Bullet_Str : aliased constant String := "Bullet";
   Burn_Str : aliased constant String := "Burn";
   Bye_Str : aliased constant String := "Bye";
   Cable_Str : aliased constant String := "Cable";
   Cake_Str : aliased constant String := "Cake";
   Called_Str : aliased constant String := "Called";
   Calls_Str : aliased constant String := "Calls";
   Can_Str : aliased constant String := "Can";
   Cancelled_Str : aliased constant String := "Cancelled";
   Candy_Str : aliased constant String := "Candy";
   Cant_Str : aliased constant String := "Cant";
   Carbon_Str : aliased constant String := "Carbon";
   Cash_Str : aliased constant String := "Cash";
   Catch_Str : aliased constant String := "Catch";
   Celebrate_Str : aliased constant String := "Celebrate";
   Celebration_Str : aliased constant String := "Celebration";
   Centuries_Str : aliased constant String := "Centuries";
   Champion_Str : aliased constant String := "Champion";
   Chaos_Str : aliased constant String := "Chaos";
   Cheap_Str : aliased constant String := "Cheap";
   Check_Str : aliased constant String := "Check";
   Checked_Str : aliased constant String := "Checked";
   Citizen_Str : aliased constant String := "Citizen";
   Clear_Str : aliased constant String := "Clear";
   Click_Str : aliased constant String := "Click";
   Clone_Str : aliased constant String := "Clone";
   Club_Str : aliased constant String := "Club";
   Combat_Str : aliased constant String := "Combat";
   Come_Str : aliased constant String := "Come";
   Computer_Str : aliased constant String := "Computer";
   Connect_Str : aliased constant String := "Connect";
   Console_Str : aliased constant String := "Console";
   Continue_Str : aliased constant String := "Continue";
   Control_Str : aliased constant String := "Control";
   Cool_Str : aliased constant String := "Cool";
   Crash_Str : aliased constant String := "Crash";
   Crew_Str : aliased constant String := "Crew";
   Damage_Str : aliased constant String := "Damage";
   Danger_Str : aliased constant String := "Danger";
   Darkness_Str : aliased constant String := "Darkness";
   Data_Str : aliased constant String := "Data";
   Day_Str : aliased constant String := "Day";
   Defects_Str : aliased constant String := "Defects";
   Delete_Str : aliased constant String := "Delete";
   Denied_Str : aliased constant String := "Denied";
   Device_Str : aliased constant String := "Device";
   Dirt_Str : aliased constant String := "Dirt";
   Do_Str : aliased constant String := "Do";
   Done_Str : aliased constant String := "Done";
   Dont_Str : aliased constant String := "Dont";
   Doubt_Str : aliased constant String := "Doubt";
   Down_Str : aliased constant String := "Down";
   Download_Str : aliased constant String := "Download";
   Dragon_Str : aliased constant String := "Dragon";
   Eight_Str : aliased constant String := "Eight";
   Everything_Str : aliased constant String := "Everything";
   Eyes_Str : aliased constant String := "Eyes";
   Fire_Str : aliased constant String := "Fire";
   First_Str : aliased constant String := "First";
   Five_Str : aliased constant String := "Five";
   For_Str : aliased constant String := "For";
   Four_Str : aliased constant String := "Four";
   Free_Str : aliased constant String := "Free";
   From_Str : aliased constant String := "From";
   Gadgets_Str : aliased constant String := "Gadgets";
   Get_Str : aliased constant String := "Get";
   Girl_Str : aliased constant String := "Girl";
   Girls_Str : aliased constant String := "Girls";
   Give_Str : aliased constant String := "Give";
   Go_Str : aliased constant String := "Go";
   Gonna_Str : aliased constant String := "Gonna";
   Good_Str : aliased constant String := "Good";
   Got_Str : aliased constant String := "Got";
   Green_Str : aliased constant String := "Green";
   Hands_Str : aliased constant String := "Hands";
   Have_Str : aliased constant String := "Have";
   Heart_Str : aliased constant String := "Heart";
   Hey_Str : aliased constant String := "Hey";
   Hold_Str : aliased constant String := "Hold";
   How_Str : aliased constant String := "How";
   Human_Str : aliased constant String := "Human";
   I_Str : aliased constant String := "I";
   If_Str : aliased constant String := "If";
   In_Str : aliased constant String := "In";
   Is_Str : aliased constant String := "Is";
   It_Str : aliased constant String := "It";
   Its_Str : aliased constant String := "Its";
   Just_Str : aliased constant String := "Just";
   Keep_Str : aliased constant String := "Keep";
   Kiss_Str : aliased constant String := "Kiss";
   Know_Str : aliased constant String := "Know";
   Lady_Str : aliased constant String := "Lady";
   Last_Str : aliased constant String := "Last";
   Lets_Str : aliased constant String := "Lets";
   Life_Str : aliased constant String := "Life";
   Like_Str : aliased constant String := "Like";
   Little_Str : aliased constant String := "Little";
   Live_Str : aliased constant String := "Live";
   Love_Str : aliased constant String := "Love";
   Loves_Str : aliased constant String := "Loves";
   Make_Str : aliased constant String := "Make";
   Maker_Str : aliased constant String := "Maker";
   Man_Str : aliased constant String := "Man";
   Me_Str : aliased constant String := "Me";
   More_Str : aliased constant String := "More";
   Music_Str : aliased constant String := "Music";
   My_Str : aliased constant String := "My";
   Night_Str : aliased constant String := "Night";
   Nine_Str : aliased constant String := "Nine";
   No_Str : aliased constant String := "No";
   Noise_Str : aliased constant String := "Noise";
   Now_Str : aliased constant String := "Now";
   Of_Str : aliased constant String := "Of";
   On_Str : aliased constant String := "On";
   One_Str : aliased constant String := "One";
   Out_Str : aliased constant String := "Out";
   Over_Str : aliased constant String := "Over";
   Point_Str : aliased constant String := "Point";
   Red_Str : aliased constant String := "Red";
   Robot_Str : aliased constant String := "Robot";
   Rock_Str : aliased constant String := "Rock";
   Roll_Str : aliased constant String := "Roll";
   Say_Str : aliased constant String := "Say";
   Seven_Str : aliased constant String := "Seven";
   Shake_Str : aliased constant String := "Shake";
   Sister_Str : aliased constant String := "Sister";
   Six_Str : aliased constant String := "Six";
   So_Str : aliased constant String := "So";
   Song_Str : aliased constant String := "Song";
   Stop_Str : aliased constant String := "Stop";
   Take_Str : aliased constant String := "Take";
   Television_Str : aliased constant String := "Television";
   Tell_Str : aliased constant String := "Tell";
   Ten_Str : aliased constant String := "Ten";
   Tension_Str : aliased constant String := "Tension";
   Terminal_Str : aliased constant String := "Terminal";
   Terror_Str : aliased constant String := "Terror";
   Test_Str : aliased constant String := "Test";
   That_Str : aliased constant String := "That";
   The_Str : aliased constant String := "The";
   Theme_Str : aliased constant String := "Theme";
   Theory_Str : aliased constant String := "Theory";
   There_Str : aliased constant String := "There";
   This_Str : aliased constant String := "This";
   Three_Str : aliased constant String := "Three";
   Throw_Str : aliased constant String := "Throw";
   Thumbs_Str : aliased constant String := "Thumbs";
   Ticket_Str : aliased constant String := "Ticket";
   Tiger_Str : aliased constant String := "Tiger";
   Time_Str : aliased constant String := "Time";
   Times_Str : aliased constant String := "Times";
   Tissue_Str : aliased constant String := "Tissue";
   To_Str : aliased constant String := "To";
   Today_Str : aliased constant String := "Today";
   Together_Str : aliased constant String := "Together";
   Tomorrow_Str : aliased constant String := "Tomorrow";
   Tonight_Str : aliased constant String := "Tonight";
   Too_Str : aliased constant String := "Too";
   Touch_Str : aliased constant String := "Touch";
   Toxic_Str : aliased constant String := "Toxic";
   Track_Str : aliased constant String := "Track";
   Trash_Str : aliased constant String := "Trash";
   Turbo_Str : aliased constant String := "Turbo";
   Turn_Str : aliased constant String := "Turn";
   Twist_Str : aliased constant String := "Twist";
   Two_Str : aliased constant String := "Two";
   Undefined_Str : aliased constant String := "Undefined";
   Unique_Str : aliased constant String := "Unique";
   United_Str : aliased constant String := "United";
   Up_Str : aliased constant String := "Up";
   Update_Str : aliased constant String := "Update";
   Upload_Str : aliased constant String := "Upload";
   Ur_Str : aliased constant String := "Ur";
   Urban_Str : aliased constant String := "Urban";
   Vice_Str : aliased constant String := "Vice";
   Victory_Str : aliased constant String := "Victory";
   Vintage_Str : aliased constant String := "Vintage";
   Virtual_Str : aliased constant String := "Virtual";
   Voice_Str : aliased constant String := "Voice";
   Volume_Str : aliased constant String := "Volume";
   Wait_Str : aliased constant String := "Wait";
   Waiting_Str : aliased constant String := "Waiting";
   Wake_Str : aliased constant String := "Wake";
   Wanna_Str : aliased constant String := "Wanna";
   Want_Str : aliased constant String := "Want";
   Wanted_Str : aliased constant String := "Wanted";
   Warning_Str : aliased constant String := "Warning";
   Watch_Str : aliased constant String := "Watch";
   Wave_Str : aliased constant String := "Wave";
   Way_Str : aliased constant String := "Way";
   We_Str : aliased constant String := "We";
   Week_Str : aliased constant String := "Week";
   Weekend_Str : aliased constant String := "Weekend";
   Weird_Str : aliased constant String := "Weird";
   Welcome_Str : aliased constant String := "Welcome";
   What_Str : aliased constant String := "What";
   When_Str : aliased constant String := "When";
   White_Str : aliased constant String := "White";
   Wicked_Str : aliased constant String := "Wicked";
   Will_Str : aliased constant String := "Will";
   Wind_Str : aliased constant String := "Wind";
   Winner_Str : aliased constant String := "Winner";
   Wish_Str : aliased constant String := "Wish";
   With_Str : aliased constant String := "With";
   Without_Str : aliased constant String := "Without";
   Woman_Str : aliased constant String := "Woman";
   World_Str : aliased constant String := "World";
   Yeah_Str : aliased constant String := "Yeah";
   Year_Str : aliased constant String := "Year";
   Yellow_Str : aliased constant String := "Yellow";
   Yes_Str : aliased constant String := "Yes";
   You_Str : aliased constant String := "You";
   Your_Str : aliased constant String := "Your";
   Zero_Str : aliased constant String := "Zero";
   Zone_Str : aliased constant String := "Zone";
   Image : constant array (WNM.Speech.Word)
     of not null access constant String
     := (
         About_Str'Access,
         Abstract_Str'Access,
         Accident_Str'Access,
         Acid_Str'Access,
         Acquired_Str'Access,
         Action_Str'Access,
         Activated_Str'Access,
         Advanced_Str'Access,
         Aerial_Str'Access,
         Again_Str'Access,
         Against_Str'Access,
         Alive_Str'Access,
         All_Str'Access,
         Allowed_Str'Access,
         Alone_Str'Access,
         Alpha_Str'Access,
         Altered_Str'Access,
         Amazing_Str'Access,
         Analyze_Str'Access,
         And_Str'Access,
         Angel_Str'Access,
         Angry_Str'Access,
         Animal_Str'Access,
         Answer_Str'Access,
         Anxiety_Str'Access,
         Any_Str'Access,
         Anymore_Str'Access,
         Arcade_Str'Access,
         Are_Str'Access,
         Arrest_Str'Access,
         Artificial_Str'Access,
         Attack_Str'Access,
         Away_Str'Access,
         Baby_Str'Access,
         Back_Str'Access,
         Bad_Str'Access,
         Based_Str'Access,
         Basic_Str'Access,
         Bass_Str'Access,
         Battery_Str'Access,
         Be_Str'Access,
         Beat_Str'Access,
         Beautiful_Str'Access,
         Beta_Str'Access,
         Better_Str'Access,
         Bike_Str'Access,
         Billion_Str'Access,
         Binary_Str'Access,
         Bird_Str'Access,
         Bite_Str'Access,
         Black_Str'Access,
         Blame_Str'Access,
         Block_Str'Access,
         Blue_Str'Access,
         Bodies_Str'Access,
         Body_Str'Access,
         Bored_Str'Access,
         Boss_Str'Access,
         Box_Str'Access,
         Boy_Str'Access,
         Brain_Str'Access,
         Brakes_Str'Access,
         Break_Str'Access,
         Broadcast_Str'Access,
         Broke_Str'Access,
         Broken_Str'Access,
         Brother_Str'Access,
         Brutal_Str'Access,
         Bubble_Str'Access,
         Buddy_Str'Access,
         Bugs_Str'Access,
         Bullet_Str'Access,
         Burn_Str'Access,
         Bye_Str'Access,
         Cable_Str'Access,
         Cake_Str'Access,
         Called_Str'Access,
         Calls_Str'Access,
         Can_Str'Access,
         Cancelled_Str'Access,
         Candy_Str'Access,
         Cant_Str'Access,
         Carbon_Str'Access,
         Cash_Str'Access,
         Catch_Str'Access,
         Celebrate_Str'Access,
         Celebration_Str'Access,
         Centuries_Str'Access,
         Champion_Str'Access,
         Chaos_Str'Access,
         Cheap_Str'Access,
         Check_Str'Access,
         Checked_Str'Access,
         Citizen_Str'Access,
         Clear_Str'Access,
         Click_Str'Access,
         Clone_Str'Access,
         Club_Str'Access,
         Combat_Str'Access,
         Come_Str'Access,
         Computer_Str'Access,
         Connect_Str'Access,
         Console_Str'Access,
         Continue_Str'Access,
         Control_Str'Access,
         Cool_Str'Access,
         Crash_Str'Access,
         Crew_Str'Access,
         Damage_Str'Access,
         Danger_Str'Access,
         Darkness_Str'Access,
         Data_Str'Access,
         Day_Str'Access,
         Defects_Str'Access,
         Delete_Str'Access,
         Denied_Str'Access,
         Device_Str'Access,
         Dirt_Str'Access,
         Do_Str'Access,
         Done_Str'Access,
         Dont_Str'Access,
         Doubt_Str'Access,
         Down_Str'Access,
         Download_Str'Access,
         Dragon_Str'Access,
         Eight_Str'Access,
         Everything_Str'Access,
         Eyes_Str'Access,
         Fire_Str'Access,
         First_Str'Access,
         Five_Str'Access,
         For_Str'Access,
         Four_Str'Access,
         Free_Str'Access,
         From_Str'Access,
         Gadgets_Str'Access,
         Get_Str'Access,
         Girl_Str'Access,
         Girls_Str'Access,
         Give_Str'Access,
         Go_Str'Access,
         Gonna_Str'Access,
         Good_Str'Access,
         Got_Str'Access,
         Green_Str'Access,
         Hands_Str'Access,
         Have_Str'Access,
         Heart_Str'Access,
         Hey_Str'Access,
         Hold_Str'Access,
         How_Str'Access,
         Human_Str'Access,
         I_Str'Access,
         If_Str'Access,
         In_Str'Access,
         Is_Str'Access,
         It_Str'Access,
         Its_Str'Access,
         Just_Str'Access,
         Keep_Str'Access,
         Kiss_Str'Access,
         Know_Str'Access,
         Lady_Str'Access,
         Last_Str'Access,
         Lets_Str'Access,
         Life_Str'Access,
         Like_Str'Access,
         Little_Str'Access,
         Live_Str'Access,
         Love_Str'Access,
         Loves_Str'Access,
         Make_Str'Access,
         Maker_Str'Access,
         Man_Str'Access,
         Me_Str'Access,
         More_Str'Access,
         Music_Str'Access,
         My_Str'Access,
         Night_Str'Access,
         Nine_Str'Access,
         No_Str'Access,
         Noise_Str'Access,
         Now_Str'Access,
         Of_Str'Access,
         On_Str'Access,
         One_Str'Access,
         Out_Str'Access,
         Over_Str'Access,
         Point_Str'Access,
         Red_Str'Access,
         Robot_Str'Access,
         Rock_Str'Access,
         Roll_Str'Access,
         Say_Str'Access,
         Seven_Str'Access,
         Shake_Str'Access,
         Sister_Str'Access,
         Six_Str'Access,
         So_Str'Access,
         Song_Str'Access,
         Stop_Str'Access,
         Take_Str'Access,
         Television_Str'Access,
         Tell_Str'Access,
         Ten_Str'Access,
         Tension_Str'Access,
         Terminal_Str'Access,
         Terror_Str'Access,
         Test_Str'Access,
         That_Str'Access,
         The_Str'Access,
         Theme_Str'Access,
         Theory_Str'Access,
         There_Str'Access,
         This_Str'Access,
         Three_Str'Access,
         Throw_Str'Access,
         Thumbs_Str'Access,
         Ticket_Str'Access,
         Tiger_Str'Access,
         Time_Str'Access,
         Times_Str'Access,
         Tissue_Str'Access,
         To_Str'Access,
         Today_Str'Access,
         Together_Str'Access,
         Tomorrow_Str'Access,
         Tonight_Str'Access,
         Too_Str'Access,
         Touch_Str'Access,
         Toxic_Str'Access,
         Track_Str'Access,
         Trash_Str'Access,
         Turbo_Str'Access,
         Turn_Str'Access,
         Twist_Str'Access,
         Two_Str'Access,
         Undefined_Str'Access,
         Unique_Str'Access,
         United_Str'Access,
         Up_Str'Access,
         Update_Str'Access,
         Upload_Str'Access,
         Ur_Str'Access,
         Urban_Str'Access,
         Vice_Str'Access,
         Victory_Str'Access,
         Vintage_Str'Access,
         Virtual_Str'Access,
         Voice_Str'Access,
         Volume_Str'Access,
         Wait_Str'Access,
         Waiting_Str'Access,
         Wake_Str'Access,
         Wanna_Str'Access,
         Want_Str'Access,
         Wanted_Str'Access,
         Warning_Str'Access,
         Watch_Str'Access,
         Wave_Str'Access,
         Way_Str'Access,
         We_Str'Access,
         Week_Str'Access,
         Weekend_Str'Access,
         Weird_Str'Access,
         Welcome_Str'Access,
         What_Str'Access,
         When_Str'Access,
         White_Str'Access,
         Wicked_Str'Access,
         Will_Str'Access,
         Wind_Str'Access,
         Winner_Str'Access,
         Wish_Str'Access,
         With_Str'Access,
         Without_Str'Access,
         Woman_Str'Access,
         World_Str'Access,
         Yeah_Str'Access,
         Year_Str'Access,
         Yellow_Str'Access,
         Yes_Str'Access,
         You_Str'Access,
         Your_Str'Access,
         Zero_Str'Access,
         Zone_Str'Access
        );
end WNM.Speech_Dictionary;
