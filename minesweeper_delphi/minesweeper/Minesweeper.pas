// Author: Brady Coles
// Lab Assignment # 5
// Minesweeper clone
unit Minesweeper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GridButton, Vcl.ExtCtrls, Math;

type
  CellContent = record
    isMine, isVisible, isFlagged : Boolean;
  end;
  Whole = 1..MaxInt;
  GridOfCells = array[1..9, 1..9] of CellContent;
  GridOfButtons = array[1..9, 1..9] of TGridButton;
  TForm1 = class(TForm)
    GridButton1 : TGridButton;
    StatusLabel: TLabel;
    Label1: TLabel;
    procedure NewGameClick(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; 
            PixelX, PixelY: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  TOP_BUFFER = 60;
  SIDE_BUFFER = 25;
  BOTTOM_BUFFER = 60;
  CELL_SIZE = 25;
var
  Form1: TForm1;
  Cells: GridOfCells;
  Buttons: GridOfButtons;
  BoardWidth, BoardHeight, NumberOfMines : Whole;

implementation

{$R *.dfm} // Include form definition

// Returns true if coordinates refer to a valid cell, false otherwise.
function IsValidCell(x, y: Integer): Boolean;
begin
  Result := (x >= 1) and (x <= BoardWidth) and (y >= 1) and (y <= BoardHeight);
end;

// Returns number of mines around a cell. Amount included cell (if cell is a mine).
function CellMineCount(x, y: Whole): Integer;
var
  i, j : Whole;
begin
  Result := 0;
  if IsValidCell(x, y) then
    for i := (x - 1) to (x + 1) do
      for j := (y - 1) to (y + 1) do
        if IsValidCell(i, j)
          then if Cells[i][j].isMine then Result := Result + 1;
end;

// Reveals the cell, giving the surrounding mine count if not a mine, or
// the '*' symbol if the cell is a mine. Also disables the button. If the cell
// has zero mines around it, reveals all surrounding cells as well (recursively).
procedure Reveal(x, y: Whole);
var
  i, j, count : Integer;
begin
  if IsValidCell(x, y) and not Cells[x][y].isVisible then
  begin
    if Cells[x][y].isMine then
    begin
      Buttons[x][y].Caption := '*';
      Buttons[x][y].Enabled := False;
      Cells[x][y].isVisible := True;
    end
    else
    begin
      count := CellMineCount(x, y);
      Buttons[x][y].Caption := count.ToString;
      Buttons[x][y].Enabled := False;
      // Before recursive call to avoid infinite recursion.
      Cells[x][y].isVisible := True;
      if count = 0 then
        // Reveal surrounding cells.
        for i := (x - 1) to (x + 1) do
          for j := (y - 1) to (y + 1) do
            if IsValidCell(i, j)
              then Reveal(i, j);
    end;
  end;
end;

// Toggles the flag on a game cell. If the cell has not been revealed, flags
// the cell if not flagged, removes flag if flagged. Flagged cells cannot be
// revealed.
procedure Flag(x, y : Integer);
begin
  if IsValidCell(x,y) and (not Cells[x][y].isVisible) then
  begin
    if Cells[x][y].isFlagged then
    begin
      Cells[x][y].isFlagged := False;
      Buttons[x][y].Caption := '';
    end
    else
    begin
      Cells[x][y].isFlagged := True;
      Buttons[x][y].Caption := 'F';
    end;
  end;
end;


// Initialized data for game board. Prepared data for window initialization.
// Randomly chooses cells to be mines.
procedure InitializeBoard(width, height, mines : Whole);
var
   rand, x, y, i, j : Whole;
begin
  BoardWidth := width;
  BoardHeight := height;
  NumberOfMines := mines;
  // Initialize data, needed for resets between games.
  for i := 1 to BoardWidth do
    for j := 1 to BoardHeight do
    begin
      Cells[i][j].isMine := False;
      Cells[i][j].isVisible := False;
      Cells[i][j].isFlagged := False;
    end;
  // Select mines
  for i := 1 to NumberOfMines do
    begin
    repeat
      rand := RandomRange(1, BoardWidth * BoardHeight);
      x := rand mod BoardWidth;
      y := rand div BoardHeight;
    until not Cells[x][y].isMine;
    Cells[x][y].isMine := True;
  end;
end;

// Set up form for a game. Makes window proper size for game board, creates
// or resets game cells.
procedure InitializeWindow;
var
  B : TGridButton;
  i, j : Whole;
begin
  // Set up window
  Form1.Width := BoardWidth * CELL_SIZE + 2 * SIDE_BUFFER
    + Form1.Margins.Left + Form1.Margins.Right + 10;
  Form1.Height := BoardHeight * CELL_SIZE + TOP_BUFFER + BOTTOM_BUFFER
    + Form1.Margins.Top + Form1.Margins.Bottom;
  Form1.StatusLabel.Caption := format('%u Mines', [NumberOfMines]);
  // Set up cells
  for i := 1 to BoardWidth do
    for j := 1 to BoardHeight do
    begin
      if (Buttons[i][j] = nil) then Buttons[i][j] := TGridButton.Create(Form1);
      Buttons[i][j].X := i;
      Buttons[i][j].Y := j;
      Buttons[i][j].Height := CELL_SIZE;
      Buttons[i][j].Width := CELL_SIZE;
      Buttons[i][j].Left := SIDE_BUFFER + (i - 1) * CELL_SIZE;
      Buttons[i][j].Top := TOP_BUFFER + (j - 1) * CELL_SIZE;
      Buttons[i][j].Caption := '';
      Buttons[i][j].Parent := Form1;
      Buttons[i][j].OnMouseDown := Form1.GridButtonClick;
      Buttons[i][j].Enabled := True;
    end;
end;

// Checks if the game has been won. Returns true if only unclicked cells
// are mines.
function CheckWinState : Boolean;
var
i, j : Integer;
begin
  for i := 1 to BoardWidth do
    for j := 1 to BoardHeight do
      if (not Cells[i][j].isVisible) and (not Cells[i][j].isMine) then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

// Ends the game by revealing all cells and changing label to win or loss message
procedure EndGame(isWin : Boolean);
var
  i, j : Whole;
begin
  for i := 1 to BoardWidth do
    for j := 1 to BoardHeight do
      Reveal(i, j);
  if isWin then Form1.StatusLabel.Caption := 'You Win!'
  else Form1.StatusLabel.Caption := 'You Lose!'
end;

// Handle starting a new game. Creates a 9x9 game with 10 mines.
procedure TForm1.NewGameClick(Sender: TObject);
begin
  InitializeBoard(9, 9, 10);
  InitializeWindow;
end;

// Handle a click on a game cell.
// Checks mouse button, right is flag, left is reveal.
// Handles ending the game on a win or loss.
procedure TForm1.GridButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; 
        PixelX, PixelY: Integer);
var
  x, y : Whole;
  count : Integer;
begin
  // Ensure that the event was on a game cell
  if Sender is TGridButton then
  begin
    x := TGridButton(Sender).X;
    y := TGridButton(Sender).Y;
    if not IsValidCell(x, y) then exit;
    case Button of
      // Reveal. Note that already revealed cells will be disabled, so all
      // events should be on unrevealed cells. If cell is revealed, Reveal
      // procedure will ignore it.
      mbLeft:
      begin
        if Cells[x][y].isFlagged then exit;
        if Cells[x][y].isMine then EndGame(False) // Game over, player clicked mine
        else
        // Reveal cell, if last cell, win game.
        begin
          Reveal(x, y);
          if CheckWinState then EndGame(True);
        end;
      end;
      // Flag or unflag cell, depending on current state.
      mbRight: Flag(x, y)
    end;
  end;
end;

end.
