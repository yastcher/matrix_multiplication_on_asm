unit MulOnAsm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    StrGrd: TStringGrid;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const ItemMax=10000;

type

   TMElement=record {элемент Матрицы связей}
      IndI:word;  {индекс I [0..N-1]}
      IndJ:word;  {индекс J [0..N-1]}
      Wes:single;    {вес связи}
              end;
    TVElement=record {элемент вектора
                       выходов нейронов}
      IndI:word;   {индекс J[0..N-1]}
      ValX:single;    {величина X-выход нейрона}
      Sum:single;     {величина суммы входов}
               end;

    Potok = TMemoryStream;{общий тип для
                         динамических массивов}
    TSpisok=class(TList); {общий тип для
                         списковых массивов}
    TSpisMelem=class    {класс элемента списка}
        Elem:TMElement;
        Constructor Create(MElem:TMElement);
        end;



var a,b,res:array [1..10000, 1..10000] of double;
    c:array [1..10000] of double;
    a_matr:TSpisok;
    b_vect:Potok;
    quanN, quanNCl, quanNN:word;

function OneMul(X, Y: double): double;
asm
  fld     X
  fmul    Y
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
quanN:=2;
quanNCl:=2-1;
a[1,1]:=1;
b[1,1]:=1;
a[1,2]:=0;
b[1,2]:=0;
a[2,1]:=0;
b[2,1]:=0;
a[2,2]:=1;
b[2,2]:=1;
end;

procedure MulMatrToMatrOnAsm;
label RowCount, ColCount, RowAndCol;
asm
   push cx
   mov cx, quanNCl
   RowCount:
      push cx
      mov di,cx
      mov cx, quanNCl
      ColCount:
         push cx
         mov si,cx
         fldz                     {0 to st}

         mov bx, quanN
         imul bx, di+1

         mov cx, quanNCl
         RowAndCol:
{            mov bx, quanN
            imul bx, di
            add bx, cx}
            fld a[bx]              {a[di] to st, 0 to st(1)}
            dec bx

            mov ax, quanN
            imul ax, cx
            add ax, si
            fld b[ax]               {b[si] to st, a[di] to st(1), 0 to st(2)}

            fmul st, st(1)             {b[si]*=a[di]}
            fadd st, st(2)             {res[]+=b[si]}
            fst st
            fst st                 {st_old==st & co-stack cleaned}
         loop RowAndCol

         imul si, quanN
         fst res[si+di]
         pop cx
      loop ColCount
      pop cx
   loop RowCount
   pop cx
end;




 {=======================================}
Constructor TSpisMElem.Create(MElem:TMElement);
   begin
   Elem:=MElem;
   inherited Create;
   end;

 {==========================================}
 {функция возвращает Элемент Вектора стоящий на
  i-м месте в массиве }
function VGetElement(i:word;Pot:Potok):TVElement;
  begin
  Pot.Position:=(i*SizeOf(TVElement));
  Pot.Read(Result,SizeOf(TVElement));
  end;

 {==========================================}
 {процедура засылает Элемент Вектора на
  i-е место в массиве }
procedure VSetElement(i:word;Elem:TVElement;Pot:Potok);
  begin
  Pot.Position:=(i*SizeOf(TVElement));
  Pot.Write(Elem,SizeOf(TVElement));
  end;

 {==========================================}
 {процедура умножения матрицы M
 на вектор V ; результат лежит
 в массиве Вектор в колонке .Sum }
Procedure MultiM_V(MMSpisok:TSpisok;VPot:Potok);
Label Metk1,Metk2;
var
Col,Row:word;
j,MaxJ:longint;
MElem:TMElement;
VElemI:TVElement;
VElemJ:TVElement;
PS:TSpisok;
begin
     Col:=0; Row:=0;
     MaxJ:=VPot.size div SizeOf(TVElement);
     {обнуление массива Sum J}
     for j:=0 to MaxJ-1 do
         begin
         VElemJ:=VGetElement(J,VPot);
         VElemJ.Sum:=0;
         VSetElement(j,VElemJ,Vpot);
         end;
     {цикл по массиву всех связей}
Metk1:
     if  Row<MMSpisok.Count then
         begin
         if Col<ItemMax then
            begin
            PS:=TSpisok(MMSpisok.Items[Row]);
            if Col<PS.Count then
               begin
               MElem:=TSpisMElem(PS.Items[Col]).Elem;
               VElemI:=VGetElement(MElem.IndI-1,VPot);
               VElemJ:=VGetElement(MElem.IndJ-1,VPot);
               VElemI.Sum:=VElemI.Sum+VElemJ.ValX*MElem.Wes;
               VSetElement(MElem.IndI-1,VElemI,VPot);
               Col:=Col+1;
               goto Metk1;
               end
            else goto Metk2;
            end
         else
             begin
             Row:=Row+1;
             Col:=0;
             goto Metk1;
             end;
         end;
Metk2:
end;

{-------------------------------}

Procedure MultiMatrToVect(MMSpisok:TSpisok;VPot:Potok);
Label Mul, ExitMul;
var
Col,Row:word;
j,MaxJ:longint;
MElem:TMElement;
VElemI:TVElement;
VElemJ:TVElement;
PS:TSpisok;
begin
     Col:=0; Row:=0;
     MaxJ:=VPot.size div SizeOf(TVElement);
     {обнуление массива Sum J}
     for j:=0 to MaxJ-1 do
         begin
         VElemJ:=VGetElement(J,VPot);
         VElemJ.Sum:=0;
         VSetElement(j,VElemJ,Vpot);
         end;
     {цикл по массиву всех связей}
Mul:









     if  Row<MMSpisok.Count then
         begin
         if Col<ItemMax then
            begin
            PS:=TSpisok(MMSpisok.Items[Row]);
            if Col<PS.Count then
               begin
               MElem:=TSpisMElem(PS.Items[Col]).Elem;
               VElemI:=VGetElement(MElem.IndI-1,VPot);
               VElemJ:=VGetElement(MElem.IndJ-1,VPot);
               VElemI.Sum:=VElemI.Sum+VElemJ.ValX*MElem.Wes;
               VSetElement(MElem.IndI-1,VElemI,VPot);
               Col:=Col+1;
               goto Mul;
               end
            else goto ExitMul;
            end
         else
             begin
             Row:=Row+1;
             Col:=0;
             goto Mul;
             end;
         end;
ExitMul:
end;


end.
