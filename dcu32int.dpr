{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$APPTYPE CONSOLE}
program dcu32int;
(*
The main module of the DCU32INT utility by Alexei Hmelnov.
----------------------------------------------------------------------------
E-Mail: alex@icc.ru
http://hmelnov.icc.ru/DCU/
----------------------------------------------------------------------------

See the file "readme.txt" for more details.

------------------------------------------------------------------------
                             IMPORTANT NOTE:
This software is provided 'as-is', without any expressed or implied warranty.
In no event will the author be held liable for any damages arising from the
use of this software.
Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:
1. The origin of this software must not be misrepresented, you must not
   claim that you wrote the original software.
2. Altered source versions must be plainly marked as such, and must not
   be misrepresented as being the original software.
3. This notice may not be removed or altered from any source
   distribution.
*)
//  {$IFNDEF LINUX}Windows,{$ELSE}LinuxFix,{$ENDIF}

uses
  SysUtils,{$IFDEF UNICODE}AnsiStrings,{$ENDIF}
  DCU32 in 'DCU32.pas',
  DCUTbl in 'DCUTbl.pas',
  DCU_In in 'DCU_In.pas',
  DCU_Out in 'DCU_Out.pas',
  FixUp in 'FixUp.pas',
  DCURecs in 'DCURecs.pas',
  DasmDefs in 'DasmDefs.pas',
  DasmCF in 'DasmCF.pas',
  DCP in 'DCP.pas',
  DasmX86 in 'DasmX86.pas',
  DasmMSIL in 'DasmMSIL.pas';

{$R *.res}

procedure WriteUsage;
begin
  Writeln(
  'Usage:'#13#10+
  '  DCU32INT <Source file> <Flags> [<Destination file>]'#13#10+
  'Source file - DCU(DPU,DCUIL) or Package (DCP,DCPIL) file'#13#10+
  'Destination file may contain * to be replaced by unit name or name and extension'#13#10+
  'Destination file = "-" => write to stdout.'#13#10+
 {$IFNDEF LINUX}
  'Flags (start with "/" or "-"):'#13#10+
 {$ELSE}
  'Flags (start with "-"):'#13#10+
 {$ENDIF}
  ' -S<show flag>* - Show flags (-S - show all), default: (+) - on, (-) - off'#13#10+
  '    A(-) - show Address table'#13#10+
  '    C(-) - don''t resolve Constant values'#13#10+
  '    D(-) - show Data block'#13#10+
  '    d(-) - show dot types'#13#10+
  '    F(-) - show Fixups'#13#10+
  '    H(+) - show Heuristic strings'#13#10+
  '    I(+) - show Imported names'#13#10+
  '    L(-) - show table of Local variables'#13#10+
  '    M(-) - don''t resolve class Methods'#13#10+
  '    O(-) - show file Offsets'#13#10+
  '    S(-) - show Self arguments of methods and 2nd call flags of `structors'#13#10+
  '    T(-) - show Type table'#13#10+
  '    U(-) - show Units of imported names'#13#10+
  '    V(-) - show auxiliary Values'#13#10+
  '    v(-) - show VMT'#13#10+
  ' -O<option>* - code generation options, default: (+) - on, (-) - off'#13#10+
  '    V(-) - typed constants as Variables'#13#10+
  '    S(-) - check unit Stamps'#13#10+
  ' -I - interface part only'#13#10+
  ' -U<paths> - Unit directories, * means autodetect by unit version'#13#10+
  ' -P<paths> - Pascal source directories (just "-P" means: "seek for *.pas in'#13#10+
  '    the unit directory"). Without this parameter src lines won''t be reported'#13#10+
  ' -R<Alias>=<unit>[;<Alias>=<unit>]* - set unit aliases'#13#10+
  ' -N<Prefix> - No Name Prefix ("%" - Scope char)'#13#10+
  ' -F<FMT> - output format (T - text (default), H-HTML)'#13#10+
  ' -D<Prefix> - Dot Name Prefix ("%" - Scope char)'#13#10+
  ' -Q<Query flag> - Query additional information.'#13#10+
  '    F(-) - class fields'#13#10+
  '    V(-) - class virtual methods'#13#10+
  ' -A<Mode> - disAssembler mode'#13#10+
  '    S(+) - simple Sequential (all memory is a sequence of ops)'#13#10+
  '    C(-) - control flow'#13#10
  );
end ;

const
  DCUName: String = '';
  FNRes: String = '';

{ Queries: }
type
  TQueryFlag = (qfFields,qfVMT);
  TQueryFlags = set of TQueryFlag;

const
  qfAll = [Low(TQueryFlag)..High(TQueryFlag)];

var
  Queries: TQueryFlags=[];

function ProcessParms: boolean;
var
  i,j: integer;
  PS: String;
  Ch: Char;
begin
  Result := false;
  for i:=1 to ParamCount do begin
    PS := ParamStr(i);
    if (Length(PS)>1)and({$IFNDEF LINUX}(PS[1]='/')or{$ENDIF}(PS[1]='-')) then begin
      Ch := UpCase(PS[2]);
      case Ch of
        'H','?': begin
          WriteUsage;
          Exit;
         end ;
        'S': begin
          if Length(PS)=2 then
            SetShowAll
          else begin
            for j:=3 to Length(PS) do begin
              Ch := {UpCase(}PS[j]{)};
              case Ch of
                'A': ShowAddrTbl := true;
                'C': ResolveConsts := false;
                'D': ShowDataBlock := true;
                'd': ShowDotTypes := true;
                'F': ShowFixupTbl := true;
                'H': ShowHeuristicRefs := false;
                'I': ShowImpNames := false;
                'L': ShowLocVarTbl := true;
                'M': ResolveMethods := false;
                'O': ShowFileOffsets := true;
                'S': ShowSelf := true;
                'T': ShowTypeTbl := true;
                'U': ShowImpNamesUnits := true;
                'V': ShowAuxValues := true;
                'v': ShowVMT := true;
              else
                Writeln('Unknown show flag: "',Ch,'"');
                Exit;
              end ;
            end ;
          end ;
        end ;
        'Q': begin
          if Length(PS)=2 then
            Queries := qfAll
          else begin
            Queries := [];
            for j:=3 to Length(PS) do begin
              Ch := {UpCase(}PS[j]{)};
              case Ch of
               'F': Include(Queries,qfFields);
               'V': Include(Queries,qfVMT);
              else
                Writeln('Unknown query flag: "',Ch,'"');
                Exit;
              end ;
            end ;
          end ;
        end ;
        'O':
          for j:=3 to Length(PS) do begin
            Ch := {UpCase(}PS[j]{)};
            case Ch of
              'V': GenVarCAsVars := true;
              'S': IgnoreUnitStamps := false;
            else
              Writeln('Unknown code generation option: "',Ch,'"');
              Exit;
            end ;
          end ;
        'I': InterfaceOnly := true;
        'U': begin
          Delete(PS,1,2);
          DCUPath := PS;
        end ;
        'R': begin
          Delete(PS,1,2);
          SetUnitAliases(PS);
        end ;
        'P': begin
          Delete(PS,1,2);
          PASPath := PS;
        end ;
        'N': begin
          Delete(PS,1,2);
          NoNamePrefix := PS;
        end ;
        'D': begin
          Delete(PS,1,2);
          DotNamePrefix := PS;
        end ;
        'A': begin
           if Length(PS)=2 then
             Ch := 'C'
           else
             Ch := UpCase(PS[3]);
           case Ch of
            'S': DasmMode := dasmSeq;
            'C': DasmMode := dasmCtlFlow;
           else
             Writeln('Unknown disassembler mode: "',Ch,'"');
             Exit;
           end ;
        end ;
        'F': begin
          if (Length(PS)>2)and(UpCase(PS[3])='H') then
            OutFmt := ofmtHTM;
        end ;
      else
        Writeln('Unknown flag: "',Ch,'"');
        Exit;
      end ;
      Continue;
    end ;
    if DCUName='' then
      DCUName := PS
    else if FNRes='' then
      FNRes := PS
    else
      Exit;
  end ;
  Result := DCUName<>'';
end ;

procedure QueryUnit(U: TUnit; Queries: TQueryFlags);
{ Output information to simplify disassembly analysis}

  function ShowFields(U1: TUnit; Hdr: AnsiString; F: TDCURec{TNameDecl}): Boolean;
  begin
    Result := false;
    while F<>Nil do begin
      if (F is TLocalDecl)and(F.GetTag=arFld) then begin
        if not Result then begin
          Result := true;
          if Hdr<>'' then begin
            Writer.NLOfs := 4;
            NL;
            PutS(Hdr);
          end ;
          Writer.NLOfs := 6;
        end ;
        NL;
        PutSFmt('@%d=$%0:x ',[TLocalDecl(F).Ndx]);
        PutS(F.Name^.GetStr);
        PutS(': ');
        U1.ShowTypeDef(TLocalDecl(F).hDT,Nil);
      end ;
      F := {TNameDecl}(F.Next);
    end ;
  end ;

  function ShowParentFields(U1: TUnit; hParent: TNDX): Boolean;
  var
    TD: TTypeDef;
    U2: TUnit;
  begin
    Result := false;
    TD := U1.GetGlobalTypeDef(hParent,U2);
    if TD=Nil then
      Exit;
    if not(TD is TRecBaseDef) then
      Exit;
    if TD is TOOTypeDef then begin
      if ShowParentFields(U2,TOOTypeDef(TD).hParent) then
        Result := true;
    end ;
    if ShowFields(U2,TD.Name^.GetStr,TRecBaseDef(TD).Fields) then
      Result := true;
  end ;

  function ShowMethods(U1: TUnit; Hdr: AnsiString; F: TDCURec{TNameDecl}): Boolean;
  begin
    Result := false;
    while F<>Nil do begin
      if (F is TMethodDecl)and(TMethodDecl(F).LocFlags and lfMethodKind in [lfVirtual])
        and(TMethodDecl(F).LocFlags and lfOverride=0{Don't show it again})
      then begin
        if not Result then begin
          Result := true;
          if Hdr<>'' then begin
            Writer.NLOfs := 4;
            NL;
            PutS(Hdr);
          end ;
          Writer.NLOfs := 6;
        end ;
        NL;
        PutSFmt('[%d=$%0:x] ',[TLocalDecl(F).hDT*4]);
        PutS(F.Name^.GetStr);
      end ;
      F := TNameDecl(F.Next);
    end ;
  end ;

  function ShowParentMethods(U1: TUnit; hParent: TNDX): Boolean;
  var
    TD: TTypeDef;
    U2: TUnit;
  begin
    Result := false;
    TD := U1.GetGlobalTypeDef(hParent,U2);
    if TD=Nil then
      Exit;
    if not(TD is TOOTypeDef) then
      Exit;
    if ShowParentMethods(U2,TOOTypeDef(TD).hParent) then
      Result := true;
    if ShowMethods(U2,TD.Name^.GetStr,TOOTypeDef(TD).Fields) then
      Result := true;
  end ;

var
  i: Integer;
  TD: TTypeDef;
  U1: TUnit;
  Hdr: AnsiString;
begin
  NL;
  PutKW('queries');
  Writer.NLOfs := 2;
  NL;
  for i:=1 to U.TypeCount do begin
    TD := U.GetGlobalTypeDef(i,U1);
    if TD=Nil then
      Continue;
    if (TD is TRecBaseDef)and((TD is TOOTypeDef)or(TD is TRecDef)) then begin
      if U1<>U then begin
        PutS(U1.UnitName);
        PutCh('.');
      end ;
      PutS(TD.Name^.GetStr);
      if qfFields in Queries then begin
        Hdr := '<FIELDS>';
        if TD is TOOTypeDef then
          {if} ShowParentFields(U1,TOOTypeDef(TD).hParent) {then
            Hdr := '<FIELDS>'};
        ShowFields(U1,Hdr,TRecBaseDef(TD).Fields);
      end ;
      if (qfVMT in Queries)and(TD is TOOTypeDef) then begin
        ShowParentMethods(U1,TOOTypeDef(TD).hParent);
        ShowMethods(U1,'<METHODS>',TOOTypeDef(TD).Fields);
      end ;
      {
  VMCnt: TNDX;//number of virtual methods
      end ;}
      Writer.NLOfs := 2;
      NL;
    end ;
  end ;
end ;


function ReplaceStar(FNRes,FN: String): String;
var
  CP: PChar;
begin
  CP := StrScan(PChar(FNRes),'*');
  if CP=Nil then begin
    Result := FNRes;
    Exit;
  end ;
  if StrScan(CP+1,'*')<>Nil then
    raise Exception.Create('2nd "*" is not allowed');
  FN := ExtractFilename(FN);
  if (CP+1)^=#0 then begin
    Result := Copy(FNRes,1,CP-PChar(FNRes))+ChangeFileExt(FN,'.int');
    Exit;
  end;
  Result := Copy(FNRes,1,CP-PChar(FNRes))+ChangeFileExt(FN,'')+Copy(FNRes,CP-PChar(FNRes)+2,MaxInt);
end ;

procedure ProcessExc(E: Exception; OutRedir: boolean);
var
  ExcS: AnsiString;
begin
  ExcS := {$IFDEF UNICODE}AnsiStrings.{$ENDIF}Format('!!!%s: "%s"',[E.ClassName,E.Message]);
  if Writer<>Nil then
    ReportExc(ExcS);
  {if TTextRec(FRes).Mode<>fmClosed then begin
    Writeln(FRes);
    Writeln(FRes,ExcS);
    Flush(FRes);
  end ;}
  if OutRedir then
    Writeln(ExcS);
end;


function ProcessUnit(FN: String; OutRedir: boolean): Integer;
var
  U: TUnit;
begin
  Result := 0;
  try
    FN := ExpandFileName(FN);
    U := Nil;
    try
      U := GetDCUByName(FN,'',0,false{Will be ignored 'cause verRq=0},
        dcuplWin32{Will be ignored 'cause verRq=0},0){TUnit.Create(FN)};
    finally
      if U=Nil then
        U := MainUnit;
      if U<>Nil then begin
        U.Show;
        if Queries<>[] then
          QueryUnit(U,Queries);
      end ;
    end ;
  except
    on E: Exception do begin
      ProcessExc(E,OutRedir);
      Result := 1;
    end ;
  end ;
end;

function ProcessFile(const FN: String): integer {ErrorLevel};
var
  NS,Ext,UnitFN: String;
  IsDCP,OutRedir: boolean;
 // CP: PChar;
  Pkg: TDCPackage;
  i: Integer;
  Writer: TBaseWriter;
begin
  Result := 0;
  OutRedir := false;
  if FNRes='-' then
    FNRes := ''
  else begin
    Writeln{(StdErr)};
    Writeln('File: "',FN,'"');
    NS := ExtractFileNamePkg(FN);
    {CP := StrScan(PChar(NS),PkgSep);
    if CP<>Nil then
      NS := StrPas(CP+1);}
    if FNRes='' then
      FNRes := ExtractFilePath(FN)+ChangeFileExt(NS,DefaultExt[OutFmt])
    else
      FNRes := ReplaceStar(FNRes,FN);
    Writeln('Result: "',FNRes,'"');
//    CloseFile(Output);
    Flush(Output);
    OutRedir := true;
  end ;
  IsDCP := IsDCPName(FN);
  Writer := Nil;
  {AssignFile(FRes,FNRes);
  TTextRec(FRes).Mode := fmClosed;}
  try
    try
      //Rewrite(FRes); //Test whether the FNRes is a correct file name
      Writer := InitOut(FNRes);
      try
        if not IsDCP then
          Result := ProcessUnit(FN,OutRedir)
        else begin
          Pkg := LoadPackage(FN,true{IsMain});
          if Pkg=Nil then
            raise Exception.CreateFmt('Error loading package "%s"',[FN]);
          Ext := ExtractFileExt(FN);
          if Length(Ext)>=4 then
            Ext[4] := 'u';
          for i:=0 to Pkg.Count-1 do begin
            UnitFN := FN+PkgSep+Pkg[i]+Ext; //It is easier to find it by name
            MainUnit := Nil;
            if ProcessUnit(UnitFN,OutRedir)<>0 then
              Result := 1;
            NL;
          end;
        end;
      finally
        FreeDCU;
      end ;
    except
      on E: Exception do begin //Catch InitOut or FreeDCU errors
        ProcessExc(E,OutRedir);
        Result := 1;
      end ;
    end ;
  finally
    if OutRedir then begin
      Writeln(Format('Total %d lines generated.',[Writer.OutLineNum]));
      Close(Output);
    end ;
    Writer.Free;
    {if TTextRec(FRes).Mode<>fmClosed then begin
      DoneOut;
      Close(FRes);
    end ;}
  end ;
end ;

begin
  {$IFDEF CONDITIONALEXPRESSIONS}
  {$IF Declared(FormatSettings)}
  FormatSettings. //Required since XE6
  {$IFEND}
  {$ENDIF}
  DecimalSeparator := '.';
  if not ProcessParms then begin
    Writeln('Call this program with -? or -h parameters for help on usage.');//WriteUsage;
    Exit;
  end ;
  Halt(ProcessFile(DCUName));
end.
