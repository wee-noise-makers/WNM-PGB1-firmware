-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with HAL;     use HAL;
with HAL.SPI; use HAL.SPI;

with RP.SPI; use RP.SPI;
with RP.DMA;
with RP.Device;
with RP.GPIO; use RP.GPIO;

package body WNM_HAL.OLED is

   Width : constant := WNM_Configuration.Screen_Width;
   Height : constant := WNM_Configuration.Screen_Height;

   SPI         : RP.SPI.SPI_Port renames RP.Device.SPI_1;
   DMA_Trigger : constant RP.DMA.DMA_Request_Trigger := RP.DMA.SPI1_TX;
   Screen_SPI_DMA : constant RP.DMA.DMA_Channel_Id := 4;

   N_Reset : RP.GPIO.GPIO_Point := (Pin => 8);
   DC      : RP.GPIO.GPIO_Point := (Pin => 9);
   --  NCS     : RP.GPIO.GPIO_Point := (Pin => 14);

   SCK     : RP.GPIO.GPIO_Point := (Pin => 10); -- D0
   MOSI    : RP.GPIO.GPIO_Point := (Pin => 11); -- D1

   subtype Framebuffer is SPI_Data_8b (0 .. (Width * (Height / 8)) - 1);
   Screen_Pixels : array (Boolean) of Framebuffer;

   Write : Boolean := True;

   procedure Initialize;
   procedure Write_Cmd (Cmd : UInt8);
   procedure Write_Cmd (Cmds : SPI_Data_8b);

   --  register definitions
   SET_CONTRAST        : constant UInt8 := 16#81#;
   SET_ENTIRE_ON       : constant UInt8 := 16#A4#;
   SET_NORM_INV        : constant UInt8 := 16#A6#;
   SET_DISP            : constant UInt8 := 16#AE#;
   SET_MEM_ADDR        : constant UInt8 := 16#20#;
   SET_COL_ADDR        : constant UInt8 := 16#21#;
   SET_PAGE_ADDR       : constant UInt8 := 16#22#;
   SET_DISP_START_LINE : constant UInt8 := 16#40#;
   SET_SEG_REMAP       : constant UInt8 := 16#A0#;
   SET_MUX_RATIO       : constant UInt8 := 16#A8#;
   SET_IREF_SELECT     : constant UInt8 := 16#AD#;
   SET_COM_OUT_DIR     : constant UInt8 := 16#C0#;
   SET_DISP_OFFSET     : constant UInt8 := 16#D3#;
   SET_COM_PIN_CFG     : constant UInt8 := 16#DA#;
   SET_DISP_CLK_DIV    : constant UInt8 := 16#D5#;
   SET_PRECHARGE       : constant UInt8 := 16#D9#;
   SET_VCOM_DESEL      : constant UInt8 := 16#DB#;
   SET_CHARGE_PUMP     : constant UInt8 := 16#8D#;

   Page_Addressing : constant Boolean := False;
   External_VCC : constant Boolean := False;

   Init_Cmds : constant SPI_Data_8b :=
     (SET_DISP, -- off
      SET_MEM_ADDR, -- address setting
      (if Page_Addressing
       then 16#10#   -- Page Addressing Mode
       else 16#00#), -- Horizontal Addressing Mode
      --  resolution and layout
      SET_DISP_START_LINE,
      SET_SEG_REMAP or 16#01#, -- column addr 127 mapped to SEG0
      SET_MUX_RATIO,
      Height - 1,
      SET_COM_OUT_DIR or 16#08#, -- scan from COM[N] to COM0
      SET_DISP_OFFSET,
      16#00#,
      SET_COM_PIN_CFG,
      (if Width > 2 * Height then 16#02# else 16#12#),
      --  timing and driving scheme
      SET_DISP_CLK_DIV,
      16#80#,
      SET_PRECHARGE,
      (if External_VCC then 16#22# else 16#F1#),
      SET_VCOM_DESEL,
      16#30#, --  0.83*Vcc
      --  n.b. specs for ssd1306 64x32 oled screens imply this should be 0x40
      --  display
      SET_CONTRAST,
      16#FF#, -- maximum
      SET_ENTIRE_ON, -- output follows RAM contents
      SET_NORM_INV, -- not inverted
      SET_IREF_SELECT,
      16#30#, -- enable internal IREF during display on
      --  charge pump
      SET_CHARGE_PUMP,
      (if External_VCC then 16#10# else 16#14#),
      SET_DISP or 16#01#);  -- display on

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      N_Reset.Configure (Output, Pull_Up);
      N_Reset.Clear;

      --  NCS.Configure (Output, Pull_Up, RP.GPIO.SPI);
      MOSI.Configure (Output, Pull_Up, RP.GPIO.SPI);
      SCK.Configure (Output, Pull_Up, RP.GPIO.SPI);

      DC.Configure (Output, Pull_Up);
      DC.Clear;

      SPI.Configure ((Role      => Master,
                      Baud      => 1_000_000,
                      Data_Size => HAL.SPI.Data_Size_8b,
                      Polarity  => Active_Low,
                      Phase     => Rising_Edge,
                      Blocking  => True,
                      Loopback  => False));

      --  Power on sequence
      N_Reset.Set;
      Delay_Microseconds (1);
      N_Reset.Clear;
      Delay_Microseconds (10);
      N_Reset.Set;
      Delay_Microseconds (10);
      Write_Cmd (SET_DISP or 16#01#);

      Write_Cmd (Init_Cmds);

      Write_Cmd ((SET_COL_ADDR, 0, Width - 1));
      Write_Cmd ((SET_PAGE_ADDR, 0, (Height / 8) - 1));

      -- DMA --
      declare
         use RP.DMA;
         Config : DMA_Configuration;
      begin
         Config.Trigger := DMA_Trigger;
         Config.Data_Size := Transfer_8;
         Config.Increment_Read := True;
         Config.Increment_Write := False;

         RP.DMA.Configure (Screen_SPI_DMA, Config);
      end;

   end Initialize;

   ---------------
   -- Write_Cmd --
   ---------------

   procedure Write_Cmd (Cmd : UInt8) is
      Status : SPI_Status;
      Data : constant SPI_Data_8b := (0 => Cmd);
   begin
      DC.Clear;
      SPI.Transmit (Data, Status);
   end Write_Cmd;

   ---------------
   -- Write_Cmd --
   ---------------

   procedure Write_Cmd (Cmds : SPI_Data_8b) is
      Status : SPI_Status;
   begin
      DC.Clear;
      SPI.Transmit (Cmds, Status);
   end Write_Cmd;

   ------------
   -- Update --
   ------------

   procedure Update is
      --  Status : SPI_Status;
   begin
      if RP.DMA.Busy (Screen_SPI_DMA) then
         --  Previous DMA transfer still in progress
         return;
      end if;

      DC.Set;
      --  SPI.Transmit (Screen_Pixels (Write), Status);
      RP.DMA.Start (Channel => Screen_SPI_DMA,
                    From    => Screen_Pixels (Write)'Address,
                    To      => SPI.FIFO_Address,
                    Count   => Screen_Pixels (Write)'Length);

      Write := not Write;
   end Update;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Screen_Pixels (Write) := (others => 0);
   end Clear;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (PX : Pix_X; PY : Pix_Y; On : Boolean := True) is
      X     : constant Natural := Natural (PX);
      Y     : constant Natural := Natural (PY);
      Index : constant Natural := X + (Y / 8) * Width;
      Byte  : UInt8 renames Screen_Pixels (Write) (Framebuffer'First + Index);
   begin
      if On then
         Byte := Byte or Shift_Left (1, Y mod 8);
      else
         Byte := Byte and not (Shift_Left (1, Y mod 8));
      end if;
   end Set_Pixel;

begin
   Initialize;
end WNM_HAL.OLED;
