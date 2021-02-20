&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model - общая модель
DEFINE CLASS Model AS CUSTOM
	PROCEDURE FormCreate
	PARAMETERS tnFormNumber
	LOCAL lnFormNumber
	EXTERNAL ARRAY oPath.taForms
		lnFormNumber = tnFormNumber
		DO CASE
			CASE lnFormNumber = 2
				oPath.taForms[ALEN(oPath.taForms,1)] = CREATEOBJECT("FileForm")

			CASE lnFormNumber = 3 &&m10870
				oPath.taForms[ALEN(oPath.taForms,1)] = CREATEOBJECT("CustomForm")
				
			CASE lnFormNumber = 4
				oPath.taForms[ALEN(oPath.taForms,1)] = CREATEOBJECT("RequisitesForm")
				
			CASE lnFormNumber = 5
				oPath.taForms[ALEN(oPath.taForms,1)] = CREATEOBJECT("IzmForm")
				
			CASE lnFormNumber = 6
				oPath.taForms[ALEN(oPath.taForms,1)] = CREATEOBJECT("PrizForm")
				
			CASE lnFormNumber = 7
				oPath.taForms[ALEN(oPath.taForms,1)] = CREATEOBJECT("CheckKdForm")
				
			CASE lnFormNumber = 8
				oPath.taForms[ALEN(oPath.taForms,1)] = CREATEOBJECT("SelectForm")
				
			CASE lnFormNumber = 9 &&m10860
				oPath.taForms[ALEN(oPath.taForms,1)] = CREATEOBJECT("CustomForm2")
		ENDCASE
		&&
		DIMENSION oPath.taForms(ALEN(oPath.taForms,1)+1)
	ENDPROC
	
	PROCEDURE FormsFillScreen
	PARAMETERS tcForm, tnPart
	LOCAL lcForm, ;
		  lnFormNumber, lnWidth, lnHeight
		lcForm = tcForm
		lnWidth = SYSMETRIC(21)
		lnHeight = SYSMETRIC(22)
	
		DO CASE
			CASE tnPart = 2
				&& form_file input
				lcForm.Left = lnWidth / 2 - lcForm.Width / 2
				lcForm.Top = lnHeight / 2 - lcForm.Height / 2
				oModel.FormMove(lcForm)
		
			CASE tnPart = 3
				&& m10870
				
			CASE tnPart = 4
				&& requisitesform
				lcForm.Left = lnWidth / 2 - lcForm.Width / 2
				lcForm.Top = lnHeight / 2 - lcForm.Height / 2
				oModel.FormMove(lcForm)
				
			CASE tnPart = 5
				&& izmform
				
			CASE tnPart = 6
				&& prizform
				lcForm.Left = lnWidth / 2 - lcForm.Width / 2
				lcForm.Top = lnHeight / 2 - lcForm.Height / 2
				oModel.FormMove(lcForm)
				
			CASE tnPart = 7
				&& checkkdform
				lcForm.Left = lnWidth / 2 - lcForm.Width / 2
				lcForm.Top = lnHeight / 2 - lcForm.Height / 2
				oModel.FormMove(lcForm)
				
			CASE tnPart = 8
				&& selectform
				LOCAL lcFormT
				lcFormT = oModel.FindForm("CustomForm")
				IF VARTYPE(lcFormT) == "O"
					lcFormT.Visible = .f.
					lcFormT.WindowState = 0
					&&
					lcFormT.Width = _SCREEN.Width * 0.995
					lcForm.Width = lcFormT.Width
					lcFormT.Height = oPath.tnScreenHeight
					lcForm.Height = oPath.tnSelectHeight
					lcFormT.Left = 0
					lcFormT.Top = 0
					lcForm.Left = lcFormT.Top
					lcForm.Top = oPath.tnSelectTop&&lcFormT.Height + 32
					lcFormT.Resize()
					lcForm.Resize()
					lcFormT.Visible = .t.
					lcForm.Visible = .t.
				ENDIF
				
			CASE tnPart = 9
				&& m10860
				
			CASE tnPart = 11
				&& helpform
				lcForm.Height = lnHeight / 4
				lcForm.Left = lnWidth / 2 - lcForm.Width / 2
				lcForm.Top = lnHeight / 2 - lcForm.Height / 2
				&& label
				lcForm.Label1.Top = 5
				lcForm.Label1.Left = 1				
				lcForm.Label2.Top = lcForm.Label1.Top				
				&& line
				lcForm.Line1.Left = 0
				lcForm.Line1.Top = lcForm.Label2.Top + lcForm.Label2.Height
				lcForm.Line2.Left = 1
				lcForm.Line3.Left = 1
				lcForm.Line4.Left = 1
				lcForm.Line5.Left = lcForm.Image1.Left + lcForm.Image1.Width
				&& img
				lcForm.Image1.Top = lcForm.Line1.Top + lcForm.Line1.Height + 2
				lcForm.Edit1.Height = lcForm.Image1.Height * 0.3		
				lcForm.Edit1.Width = lcForm.Image1.Width
				lcForm.Edit1.Top = lcForm.Image1.Top + lcForm.Image1.Height + 2
				lcForm.Edit1.Left = 1
				&& form
				lcForm.Height = lcForm.Label1.Height + 2 + lcForm.Image1.Height + 2 + ;
				+ lcForm.Edit1.Height + 2 + 5
				lcForm.Width = 1 + lcForm.Image1.Width + 1
				lcForm.Line1.Width = lcForm.Width - 1
				lcForm.Label2.Left = lcForm.Width - lcForm.Label2.Width - 1
				lcForm.Edit1.Width = lcForm.Width - 2
				&&
				lcForm.Line3.Top = lcForm.Image1.Top + lcForm.Image1.Height
				lcForm.Line3.Width = lcForm.Width - 2
				lcForm.Line4.Height = lcForm.Image1.Height + 1
				lcForm.Line4.Left = lcForm.Image1.Left
				lcForm.Line4.Top = lcForm.Image1.Top - 1
				lcForm.Line5.Height = lcForm.Image1.Height + 1
				lcForm.Line5.Left = lcForm.Image1.Left + lcForm.Image1.Width
				lcForm.Line5.Top = lcForm.Image1.Top - 1
				&&
				lcForm.tnHeight = lcForm.Height
				lcForm.tnWidth = lcForm.Width
				lcForm.taControlsInit()&&i,j=1name, j=2height, j=3width, j=4top, j=5left
				oModel.FormMove(lcForm)
		ENDCASE
	ENDPROC
	
	PROCEDURE FormMove
	PARAMETERS tcForm
	LOCAL lnLeft,lnTop
		lnLeft = SYSMETRIC(21) / 2 - tcForm.Width / 2
		lnTop = (SYSMETRIC(22) * 0.92 - tcForm.Height)/2
		&&
		tcForm.Left = lnLeft
		tcForm.Top = lnTop
	ENDPROC
	
	PROCEDURE FindForm
	PARAMETERS tcName
	LOCAL lcForm
		lcForm = .f.
		FOR EACH loNode IN Application.Forms
			IF ALLTRIM(UPPER(loNode.Class)) == UPPER(tcName)
				lcForm = loNode
			ENDIF
		ENDFOR
		RETURN lcForm
	ENDPROC
	
	PROCEDURE FindClass
	PARAMETERS tcName
		lcForm = .f.
		FOR EACH loNode IN Application.Objects
			IF ALLTRIM(UPPER(loNode.Class)) == UPPER(tcName) AND ;
			ALLTRIM(UPPER(loNode.BaseClass)) == ALLTRIM(UPPER("custom"))
				lcForm = loNode
			ENDIF
		ENDFOR
		RETURN lcForm
	ENDPROC
	
	PROCEDURE SetCodePage
	PARAMETERS tcFileName,tcAlias
	LOCAL lcFileName, ;
		lnWorkArea,lnWorkAreaT
		&&
		lnWorkArea = oModel.IsUsed(tcFileName)
		lnWorkAreaT = oModel.IsUsed(tcAlias)
		lnWorkArea = oModel.UseIn(lnWorkArea,lnWorkAreaT)&& выгрузка таблиц
		
		IF lnWorkArea = 0
			SELECT 0
			USE (tcFileName) ALIAS "codepage"
			SELECT "codepage"
			IF CPDBF("codepage") != 866
				DO cpzero WITH "codepage",0
				DO cpzero WITH "codepage",866
			ENDIF
			&&
			lnWorkArea = oModel.IsUsed("codepage")
			IF lnWorkArea > 0
				USE IN (lnWorkArea)
				RETURN .t.
			ENDIF
			RETURN .f.
		ENDIF		
	ENDPROC

	PROCEDURE SetHotKeys
		PUSH KEY CLEAR
		ON KEY LABEL ESC oModel.HotKeys()
		ON KEY LABEL ENTER oModel.HotKeys()
		ON KEY LABEL SHIFT+F10 oModel.DoContextMenu()
		ON KEY LABEL CTRL+A oModel.HotKeys()
		ON KEY LABEL CTRL+F oModel.HotKeys()
		ON KEY LABEL CTRL+C oModel.HotKeys()&&oModel.GridOrText(3)
		ON KEY LABEL CTRL+V oModel.HotKeys()&&oModel.GridOrText(22)		
		ON KEY LABEL CTRL+X oModel.HotKeys()

		*ON KEY LABEL CTRL+Z oModel.HotKeys()		
*!*			ON KEY LABEL CTRL+W oModel.HotKeys()
*!*			ON KEY LABEL F1 oModel.HotKeys()
*!*			ON KEY LABEL F2 oModel.HotKeys()
*!*			ON KEY LABEL F3 oModel.HotKeys()
*!*			ON KEY LABEL F4 oModel.HotKeys()
		ON KEY LABEL F5 oModel.HotKeys()
		ON KEY LABEL CTRL+F12 oModel.HotKeys()&&
*!*			ON KEY LABEL F6 oModel.HotKeys()
		ON KEY LABEL F11 oModel.HotKeys()
		ON KEY LABEL F12 oModel.HotKeys()
	ENDPROC
	
	PROCEDURE HotKeys
	LOCAL lcForm, lcAlias, ;
		lnAnswer
		lnAnswer = LASTKEY()
		lcForm = _SCREEN.ActiveForm
		lcAlias = _SCREEN.ActiveForm.Name
		&&
		IF VARTYPE(lcForm) == "O"
			IF lnAnswer = 28 
				&& f1
				*oPath.Menu1Pad3Bar1()
			ENDIF
			&&
			DO CASE
				CASE ALLTRIM(UPPER(lcAlias)) == UPPER("fileform")
					IF lcForm.Visible
						DO CASE
							CASE lcForm.Container1.Visible
								IF lnAnswer = 13
									&& enter 1 = выбор пункта 1.m10870
									oMContainerEvent.Label2Click() && открыть
								ENDIF
								
								IF lnAnswer = 27
									&& esc 2							
									MyExit() && выйти из программы
								ENDIF
																
							CASE lcForm.Container2.Visible
								IF lnAnswer = 13
									&& enter 1
									oFContainerEvent.Command1Click() && открыть
								ENDIF
								
								IF lnAnswer = 27
									&& esc 2							
									oFContainerEvent.Label3Click() && назад
								ENDIF
								
								IF lnAnswer = -4
									&& f5 3
									oFContainerEvent.Label3Click() && назад
								ENDIF
						ENDCASE											
					ENDIF
				
				CASE ALLTRIM(UPPER(lcAlias)) == UPPER("customform")
					IF lcForm.Visible				
						IF lnAnswer = 6
							&& ctrl+f 3
							*lcForm.tnKeyPress = 1&&upd		
							lcForm.Container1.Text1.Click()
							lcForm.Container1.Text1.SetFocus()
						ENDIF
						&& ctrl+a ctrl+c ctrl+v ctrl+x cretl+z
						oModel.MyContextMenu(lnAnswer)&&upd
																				
						IF lnAnswer = 13
							&& enter 4								
							IF lcForm.tlSearch
								lcForm.Container1.Command1.Click() && поиск по таблице
							ELSE
								KEYBOARD '{TAB}'
							ENDIF
						ENDIF
						
						IF lnAnswer = 27
							&& esc 5
							&& определить если активные дочерние формы на экране
							LOCAL lnMyCnt
							lnMyCnt = 0
							FOR EACH myobject IN Application.Objects &&upd
								IF UPPER(ALLTRIM(myobject.BaseClass)) == UPPER("form") AND ;
								UPPER(ALLTRIM(myobject.Class)) != UPPER("customform")
									formdelete(myobject.Class)
									lnMyCnt = lnMyCnt + 1
								ENDIF
							ENDFOR
							&&
							IF lnMyCnt > 0
								MyExit() && выйти из программы								
							ENDIF
						ENDIF
						
						IF lnAnswer = 138
							&& req 6
							oPath.DoMenu(1,2,3)
						ENDIF										
					ENDIF
					
				CASE ALLTRIM(UPPER(lcAlias)) == UPPER("requisitesform")
					IF lcForm.Visible
						IF lnAnswer = 133
							&& f11 1
							oPath.SetMyF11F12(.t.,.f.,"rqcursor",2)
						ENDIF
						
						IF lnAnswer = 134
							&& f12 2
							oPath.SetMyF11F12(.f.,.t.,"rqcursor",2)
						ENDIF
						
						IF lnAnswer = 27
							&& esc 3
							lcForm.QueryUnload()&& назад													
						ENDIF
					ENDIF
					
				CASE ALLTRIM(UPPER(lcAlias)) == UPPER("izmform")
					IF lcForm.Visible
						IF lnAnswer = 27
							&& esc 1							
							lcForm.QueryUnload()&& назад													
						ENDIF
					ENDIF
					
				CASE ALLTRIM(UPPER(lcAlias)) == UPPER("prizform")
					IF lcForm.Visible
						IF lnAnswer = 133
							&& f11 1
							oPath.SetMyF11F12(.t.,.f.,"pcursor",1)
						ENDIF
						
						IF lnAnswer = 134
							&& f12 2
							oPath.SetMyF11F12(.f.,.t.,"pcursor",1)
						ENDIF
					
						IF lnAnswer = 27
							&& esc 3
							lcForm.QueryUnload()&& назад													
						ENDIF
					ENDIF	
					
				CASE ALLTRIM(UPPER(lcAlias)) == UPPER("checkkdform")
					IF lcForm.Visible					
						IF lnAnswer = 27
							&& esc 1
							lcForm.QueryUnload()&& назад													
						ENDIF
					ENDIF
					
					IF lnAnswer = 13
						&& enter 2						
						lcForm.Container1.Command1.Click() && поиск по таблице
					ENDIF
					
				CASE ALLTRIM(UPPER(lcAlias)) == UPPER("selectform")
					IF lcForm.Visible					
						IF lnAnswer = 27
							&& esc 1
							lcForm.QueryUnload()&& назад													
						ENDIF
						
						DO CASE
							CASE lcForm.Container1.Visible						
								IF lnAnswer = 133
									&& f11 2
									oPath.SetMyF11F12(.t.,.f.,"kdtable1",1)
								ENDIF
								
								IF lnAnswer = 134
									&& f12 3
									oPath.SetMyF11F12(.f.,.t.,"kdtable1",1)
								ENDIF
								
							CASE lcForm.Container2.Visible
								IF lnAnswer = 6
									&& ctrl+f 3
									*lcForm.tnKeyPress = 1&&upd		
									lcForm.Container2.Text1.Click()
									lcForm.Container2.Text1.SetFocus()
								ENDIF
								&& ctrl+a ctrl+c ctrl+v ctrl+x cretl+z
								oModel.MyContextMenu(lnAnswer)&&upd
																						
								IF lnAnswer = 13
									&& enter 4								
									IF lcForm.tlSearch
										lcForm.Container2.Command1.Click() && поиск по таблице
									ELSE
										KEYBOARD '{TAB}'
									ENDIF
								ENDIF
						ENDCASE
					ENDIF
					
				CASE ALLTRIM(UPPER(lcAlias)) == UPPER("customform2")
*!*						IF lcForm.Visible				
*!*							IF lnAnswer = 6
*!*								&& ctrl+f 3
*!*								*lcForm.tnKeyPress = 1&&upd		
*!*								lcForm.Container1.Text1.Click()
*!*								lcForm.Container1.Text1.SetFocus()
*!*							ENDIF
*!*							&& ctrl+a ctrl+c ctrl+v ctrl+x cretl+z
*!*							oModel.MyContextMenu(lnAnswer)&&upd
*!*																					
*!*							IF lnAnswer = 13
*!*								&& enter 4								
*!*								IF lcForm.tlSearch
*!*									lcForm.Container1.Command1.Click() && поиск по таблице
*!*								ELSE
*!*									KEYBOARD '{TAB}'
*!*								ENDIF
*!*							ENDIF
*!*							
*!*							IF lnAnswer = 27
*!*								&& esc 5
*!*								&& определить если активные дочерние формы на экране
*!*								LOCAL lnMyCnt
*!*								lnMyCnt = 0
*!*								FOR EACH myobject IN Application.Objects &&upd
*!*									IF UPPER(ALLTRIM(myobject.BaseClass)) == UPPER("form") AND ;
*!*									UPPER(ALLTRIM(myobject.Class)) != UPPER("customform")
*!*										formdelete(myobject.Class)
*!*										lnMyCnt = lnMyCnt + 1
*!*									ENDIF
*!*								ENDFOR
*!*								&&
*!*								IF lnMyCnt > 0
*!*									MyExit() && выйти из программы								
*!*								ENDIF
*!*							ENDIF
*!*							
*!*							IF lnAnswer = 138
*!*								&& req 6
*!*								oPath.DoMenu(1,2,3)
*!*							ENDIF										
*!*						ENDIF								
															
*!*					CASE ALLTRIM(UPPER(lcAlias)) == UPPER("helpform")
*!*						DO CASE	
*!*							CASE lnAnswer = 27
*!*								&& esc
*!*								lcForm.QueryUnload()
*!*						ENDCASE	
			ENDCASE
		ENDIF
	ENDPROC
	
	PROCEDURE DoContext()
		DEFINE POPUP GridMenu SHORTCUT RELATIVE FROM MROW(),MCOL()
		DEFINE BAR 1 OF GridMenu PROMPT 'Копировать'
		DEFINE BAR 2 OF GridMenu PROMPT 'Вставить'
		DEFINE BAR 3 OF GridMenu PROMPT 'Вырезать'
		DEFINE BAR 4 OF GridMenu PROMPT 'Выделить все'
		ON SELECTION BAR 1 OF GridMenu oModel.MyContextMenu(3)
		ON SELECTION BAR 2 OF GridMenu oModel.MyContextMenu(22)
		ON SELECTION BAR 3 OF GridMenu oModel.MyContextMenu(24)
		ON SELECTION BAR 4 OF GridMenu oModel.MyContextMenu(1)
		ACTIVATE POPUP GridMenu
		RELEASE POPUPS GridMenu
	ENDPROC
	
	PROCEDURE DoText()
		DEFINE POPUP TextMenu SHORTCUT RELATIVE FROM MROW(),MCOL()
		DEFINE BAR 1 OF TextMenu PROMPT 'Копировать'
		DEFINE BAR 2 OF TextMenu PROMPT 'Вставить'
		DEFINE BAR 3 OF TextMenu PROMPT 'Вырезать'				
		DEFINE BAR 4 OF TextMenu PROMPT 'Выделить все'
		ON SELECTION BAR 1 OF TextMenu oModel.MyContextMenu(3)
		ON SELECTION BAR 2 OF TextMenu oModel.MyContextMenu(22)
		ON SELECTION BAR 3 OF TextMenu oModel.MyContextMenu(24)
		ON SELECTION BAR 4 OF TextMenu oModel.MyContextMenu(1)
		ACTIVATE POPUP TextMenu
		RELEASE POPUPS TextMenu
	ENDPROC
	
	PROCEDURE MyContextMenu()
	PARAMETERS tnKeyCode
	LOCAL lcForm,lcFormT,loMyGrid
		lcForm = _SCREEN.ActiveForm
		lcFormT = THIS.GetMyObject(lcForm)&&upd
		
		FOR EACH myobject IN lcForm.Objects
			IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
				IF myobject.Visible
					loMyGrid = myobject
					EXIT
				ENDIF
			ENDIF
		ENDFOR
		
		IF VARTYPE(loMyGrid) == "O"		
			IF tnKeyCode = 3
				&& ctrl+c
				IF oModel.GridOrText()
					loMyGrid.MyKeyPress(tnKeyCode)&&upd
				ELSE
					lcFormT.Text1MyKeyPress(tnKeyCode)&&oCContainerEvent
				ENDIF								
			ENDIF
			
			IF tnKeyCode = 22
				&& ctrl+v
				IF THIS.GridOrText()
					loMyGrid.MyKeyPress(tnKeyCode)&&upd
				ELSE
					lcFormT.Text1MyKeyPress(tnKeyCode)
				ENDIF
			ENDIF
			
			IF tnKeyCode = 1
				&& ctrl+a
				IF THIS.GridOrText()
					loMyGrid.MyKeyPress(tnKeyCode)&&upd
				ELSE
					lcFormT.Text1MyKeyPress(tnKeyCode)
				ENDIF
			ENDIF
			
			IF tnKeyCode = 24
				&& ctrl+x
				IF THIS.GridOrText()
					loMyGrid.MyKeyPress(tnKeyCode)&&upd
				ELSE
					lcFormT.Text1MyKeyPress(tnKeyCode)
				ENDIF
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE GetMyObject()
	PARAMETERS tcForm
	LOCAL lcObjectName
		lcObjectName = tcForm.Name
		DO CASE
			CASE UPPER(ALLTRIM(lcObjectName)) == UPPER("FileForm")
				IF tcForm.Container1.Visible
					RETURN oMContainerEvent
				ENDIF
				
				IF tcForm.Container2.Visible
					RETURN oFContainerEvent
				ENDIF
			
			CASE UPPER(ALLTRIM(lcObjectName)) == UPPER("CustomForm")
				RETURN oCContainerEvent			
			
			CASE UPPER(ALLTRIM(lcObjectName)) == UPPER("RequisitesForm")
				RETURN oRContainerEvent
			
			CASE UPPER(ALLTRIM(lcObjectName)) == UPPER("IzmForm")
				RETURN oIContainerEvent	
			
			CASE UPPER(ALLTRIM(lcObjectName)) == UPPER("PrizForm")
				RETURN oPContainerEvent
			
			CASE UPPER(ALLTRIM(lcObjectName)) == UPPER("CheckKdForm")
				RETURN oCKContainerEvent
			
			CASE UPPER(ALLTRIM(lcObjectName)) == UPPER("SelectForm")
				IF tcForm.Container1.Visible
					RETURN oSContainerEvent
				ENDIF
				
				IF tcForm.Container2.Visible
					RETURN oS2ContainerEvent
				ENDIF
				
			CASE UPPER(ALLTRIM(lcObjectName)) == UPPER("CustomForm2")
				RETURN oC2ContainerEvent		
		ENDCASE		
	ENDPROC
			
	PROCEDURE DoContextMenu()	
		IF THIS.GridOrText()
			THIS.DoContext()
		ELSE
			THIS.DoText()
		ENDIF
	ENDPROC
	
	PROCEDURE GridOrText()
	LOCAL lcForm,lcName
		lcForm = _SCREEN.ActiveForm
		lcName = lcForm.Name
		IF ALLTRIM(UPPER(lcForm.Name)) == ALLTRIM(UPPER(lcName))							
			FOR EACH myobject IN lcForm.Controls
				DO CASE
					CASE ALLTRIM(UPPER(myobject.BaseClass)) == ALLTRIM(UPPER("grid"))
						IF TYPE("lcForm.ActiveControl.Name") != "U"
							IF AT(ALLTRIM(UPPER("grid")),UPPER(lcForm.ActiveControl.Name)) > 0
								RETURN .t.
							ENDIF
						ENDIF
				 
					CASE ALLTRIM(UPPER(myobject.BaseClass)) == ALLTRIM(UPPER("container"))
						FOR EACH mycontrol IN myobject.Controls
							IF TYPE("lcForm.ActiveControl.Name") != "U"
								IF AT(ALLTRIM(UPPER("text")),UPPER(lcForm.ActiveControl.Name)) > 0
									RETURN .f.
								ENDIF
							ENDIF							
						ENDFOR											
				ENDCASE
			ENDFOR
		ENDIF
	ENDPROC
	
	PROCEDURE MyObject()
	LOCAL lcForm,lcName
		lcForm = _SCREEN.ActiveForm
		lcName = lcForm.Name
		&&
		IF ALLTRIM(UPPER(lcForm.Name)) == ALLTRIM(UPPER(lcName))							
			FOR EACH myobject IN lcForm.Controls
				IF ALLTRIM(UPPER(myobject.BaseClass)) == ALLTRIM(UPPER("container"))
					FOR EACH mycontrol IN myobject.Controls
						IF TYPE("lcForm.ActiveControl.Name") != "U"
							RETURN lcForm.ActiveControl
						ENDIF
					ENDFOR
				ELSE
					&& other
					IF TYPE("lcForm.ActiveControl.Name") != "U"
						RETURN lcForm.ActiveControl
					ENDIF											
				ENDIF				
			ENDFOR
		ENDIF
	ENDPROC
	
	PROCEDURE CreateModels()
	PUBLIC oCell,oModel_file,oModel_m10870,oModel_m10860,;
	oModel_req,oModel_izm,oModel_priz,oModel_ck,oModel_select,oModel_help
		oCell = CREATEOBJECT("Cell")
		oModel_file = CREATEOBJECT("Model_File")
		oModel_m10870 = CREATEOBJECT("Model_m10870")
		oModel_req = CREATEOBJECT("Model_req")
		oModel_izm = CREATEOBJECT("Model_izm")
		oModel_priz = CREATEOBJECT("Model_priz")
		oModel_ck = CREATEOBJECT("Model_ck")
		oModel_select = CREATEOBJECT("Model_select")
		oModel_m10860 = CREATEOBJECT("Model_m10860")
*!*			oModel_scust = CREATEOBJECT("Model_Scust")
*!*			oModel_help = CREATEOBJECT("Model_help")
	ENDPROC
	
	PROCEDURE ReleaseModels()
		RELEASE oCell,oModel_file,oModel_m10870,oModel_m10860,;
		oModel_req,oModel_izm,oModel_priz,oModel_ck,oModel_select,oModel_help
	ENDPROC

	PROCEDURE DeleteTmp
	ENDPROC
	
	PROCEDURE IsUsed()
	PARAMETERS tcValue
	LOCAL lcShort,lnWorkArea
	&& Проверка, занята ли таблица. Возвращает номер рабочего пространства, 0 - таблица не занята. Передавать можно полный путь/имя файла/алиас.
		lcShort = UPPER(JUSTSTEM(tcValue)) && получить имя файла без расширения. Короткое имя и полное дают разный результат в used().
		DIMENSION laArray(1)
		lnWorkArea = 0
		IF !EMPTY(lcShort)			
			IF AUSED(laArray) > 0				
				FOR i = 1 TO ALEN(laArray,1)
					IF ALLTRIM(UPPER(laArray[i,1])) == ALLTRIM(UPPER(lcShort))
						lnWorkArea = laArray[i,2] && номер						
						EXIT
					ENDIF									
				ENDFOR
			ENDIF
		ENDIF
		RETURN lnWorkArea
	ENDPROC
	
	PROCEDURE UseIn()
	PARAMETERS tcName,tcAlias
	LOCAL lnWorkArea,lnWorkAreaT
		lnWorkArea = THIS.IsUsed(tcName)
		lnWorkAreaT = THIS.IsUsed(tcAlias)
		
		IF lnWorkArea > 0
			SELECT (lnWorkArea)
			IF USED(lnWorkArea)
				USE IN (lnWorkArea)
			ENDIF
		ENDIF
		&&
		IF lnWorkAreaT > 0
			SELECT (lnWorkAreaT)
			IF USED(lnWorkAreaT)
				USE IN (lnWorkAreaT)
			ENDIF
		ENDIF
		
		&& 1 > 0 AND 2 <= 0
		IF lnWorkArea > 0 AND lnWorkAreaT <= 0
			RETURN lnWorkArea
		ENDIF
		
		&& 1 <= 0 AND 2 > 0
		IF lnWorkArea <= 0 AND lnWorkAreaT > 0
			RETURN lnWorkAreaT
		ENDIF
		
		&& 1 > 0 AND 2 > 0
		IF lnWorkArea > 0 AND lnWorkAreaT > 0
			RETURN lnWorkAreaT
		ENDIF
		
		&& 1 <= 0 AND 2 <= 0
		IF lnWorkArea <= 0 AND lnWorkAreaT <= 0
			RETURN 0
		ENDIF
	ENDPROC
	
	PROCEDURE CompareWorkArea()
	PARAMETERS tcName,tcAlias
	LOCAL lnWorkArea,lnWorkAreaT
		lnWorkArea = THIS.IsUsed(tcName)
		lnWorkAreaT = THIS.IsUsed(tcAlias)
		
		&& 1 > 0 AND 2 <= 0
		IF lnWorkArea > 0 AND lnWorkAreaT <= 0
			RETURN lnWorkArea
		ENDIF
		
		&& 1 <= 0 AND 2 > 0
		IF lnWorkArea <= 0 AND lnWorkAreaT > 0
			RETURN lnWorkAreaT
		ENDIF
		
		&& 1 > 0 AND 2 > 0
		IF lnWorkArea > 0 AND lnWorkAreaT > 0
			RETURN lnWorkAreaT
		ENDIF
		
		&& 1 <= 0 AND 2 <= 0
		IF lnWorkArea <= 0 AND lnWorkAreaT <= 0
			RETURN 0
		ENDIF
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_File - модель для класса формы FileForm
DEFINE CLASS Model_File AS CUSTOM
	PROCEDURE Start()
	PARAMETERS tcPre,tnForm
	LOCAL lcForm
		&& Создание формы
		lcForm = oModel.FindForm("FileForm")
		IF VARTYPE(lcForm) != "O"
			oModel.FormCreate(2) && fileform
		ENDIF
		&&
		lcForm = oModel.FindForm("FileForm")
		oModel.FormsFillScreen(lcForm, 2)
		lcForm.FormSettings(1)
		lcForm.tcPre = tcPre
		lcForm.Caption = tcPre
		&&
		lcForm.Visible = .t.
	ENDPROC
	
	PROCEDURE Start2()
	PARAMETERS tcPre,tnForm
	LOCAL lcForm
		&& fileform
		lcForm = oModel.FindForm("FileForm")
		lcForm.FormSettings(2)
		&&
		DO CASE
			CASE tnForm = 1
				&& 1
				lcForm.Container2.Text1.Value = "C:\FOXPRO2\M10870.DBF"
			
			CASE tnForm = 2
				&& 2
				lcForm.Container2.Text1.Value = "C:\FOXPRO2\M10860.DBF"

			CASE tnForm = 3
				&& 3
				lcForm.Container2.Text1.Value = "C:\FOXPRO2\M10880.DBF"

			CASE tnForm = 4
				&& 4
				lcForm.Container2.Text1.Value = "C:\FOXPRO2\M10881.DBF"

			CASE tnForm = 5
				&& 5
				lcForm.Container2.Text1.Value = "C:\FOXPRO2\M10881K.DBF"
		ENDCASE
		&&
		oPath.tnForm = tnForm
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_M10870 - модель для класса формы CustomForm (form_one)
DEFINE CLASS Model_M10870 AS CUSTOM
	PROCEDURE Start
	LOCAL lcForm, lcTabname
		&& formcreate
		oModel_m10870.FormCreate()
		lcForm = oModel.FindForm("CustomForm")
		lcTabname = oPath.tcPathFile
		&&
		IF FILE(UPPER(lcTabname)) AND VARTYPE(lcForm) == "O"
			oModel_m10870.ReadUpdate()
		ENDIF
	ENDPROC
	
	PROCEDURE ReadUpdate
	LOCAL lcTabname, lcForm, ;
		  lnFormNumber
		&& customform
		oModel_m10870.SelectData()
		oModel_m10870.FormFill()
	ENDPROC
	
	PROCEDURE FormCreate
	LOCAL lcForm
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) != "O"
			oModel.FormCreate(3)
		ENDIF
	ENDPROC
	
	PROCEDURE FormFill
	LOCAL lcForm
		lcForm = oModel.FindForm("CustomForm")
		oModel.FormsFillScreen(lcForm, 3)
	ENDPROC
	
	PROCEDURE SelectData
	LOCAL lcTabname, lcAliasname, lcAliasname1, lcForm, ;
	lcFormT
		&& form_one
		lcForm = oModel.FindForm("CustomForm")
		lcTabname = oPath.tcPathFile
		lcAliasname = lcForm.tcAlias&&JUSTSTEM(oPath.tcPathFile)
		lcAliasname1 = oPath.tcVersionName&&"m22101"&&lcAliasname + ALLTRIM("1") && Алиас таблицы
		&&
		LOCAL lcPathFolder,lbFind&&upd
		lbFind = .f.
		SET DEFAULT TO C:\FOXPRO2
		DIMENSION laArray(1)
		ADIR(laArray,"*")			
		&&
		FOR i = 1 TO ALEN(laArray,1)
			FOR j = 1 TO ALEN(laArray,2)
				IF ALLTRIM(UPPER(laArray[i,1])) == ALLTRIM(UPPER(JUSTSTEM(lcTabname) + ".dbf"))
					lcForm.tdDate = laArray[i,3] &&дата последней модификации
					lcForm.tcTime = laArray[i,4] &&время
					lbFind = .t.
				ENDIF
				EXIT
			ENDFOR
			IF lbFind
				EXIT
			ENDIF
		ENDFOR
		&&
		LOCAL lnWorkArea
		lnWorkArea = oModel.UseIn(lcTabname,lcAliasname)
		&&
		SELECT 0
		USE (lcTabname) ALIAS "rtable" EXCLUSIVE
		&&
		PUBLIC laKdArray &&
		DIMENSION laKdArray(1) &&
		SELECT "rtable"
		*SET ORDER TO kdn
		SELECT DISTINCT SPACE(2) AS 'str',kd FROM (lcForm.tcAlias) INTO ARRAY laKdArray WHERE !EMPTY(kd) &&
		CREATE CURSOR "kcursor" (str c(2),kd c(2))
		INSERT INTO "kcursor" FROM ARRAY laKdArray
		oModel_req.PreSelectData()&&upd
		&&
		lcForm.Container1.AddMyItem()
		oModel_m10870.CreateBackupTable()&&upd
		oModel_m10870.TableOptions()
		*oModel_m10870.CreateBackupTable()
		THIS.IndexCursor(2)&&nizd
		GO TOP IN (lcAliasname)				
		lcForm.Visible = .t.&&upd:перемещаем сюда
		lcForm.Resize()
		lcForm.Caption = "1 | " + lcForm.tcPre + " | Изменение: " + ALLTRIM(DTOC(lcForm.tdDate)) + " " + ALLTRIM(lcForm.tcTime) + " | Кол-во строк: " + ;
		ALLTRIM(STR(RECCOUNT(lcAliasname)))&&upd
		SET MARK OF BAR 1 OF Pad1 TO .t.&&upd
		oPath.MenuAddBar()&&upd
	ENDPROC
	
	PROCEDURE TableOptions
	LOCAL lcTabname, lcAliasname, lcForm
		lcForm = oModel.FindForm("customform")
		lcTabname = oPath.tcPathFile
		lcAliasname = "rtable"
		&& Grid1
		SELECT "rtable"
		lcForm.Grid1.ColumnCount = -1	
		lcForm.Grid1.RecordSourceType = 1
		lcForm.Grid1.RecordSource = ""
		lcForm.Grid1.RecordSource = "rtable"
		SET DATE GERMAN  && DMY
		SET HOURS TO 24 &&upd
		DIMENSION laFields(1)	  
		LOCAL lnResCnt&&upd
		lnResCnt = AFIELDS(laFields)
		
		&& Columns
		lcForm.Grid1.Column1.ControlSource = UPPER("rtable.izm") &&c		
		lcForm.Grid1.Column2.ControlSource = UPPER("rtable.nizd") &&c
		lcForm.Grid1.Column3.ControlSource = UPPER("rtable.snizd") &&c
		lcForm.Grid1.Column4.ControlSource = UPPER("rtable.mod") &&c
		lcForm.Grid1.Column5.ControlSource = UPPER("rtable.kudar") &&c
		lcForm.Grid1.Column6.ControlSource = UPPER("rtable.kuda") &&c
		lcForm.Grid1.Column7.ControlSource = UPPER("rtable.dtv") &&d
		lcForm.Grid1.Column8.ControlSource = UPPER("rtable.cex") &&c
		lcForm.Grid1.Column9.ControlSource = UPPER("rtable.rank") &&c
		lcForm.Grid1.Column10.ControlSource = UPPER("rtable.kd") &&c
		lcForm.Grid1.Column11.ControlSource = UPPER("rtable.priz") &&c
		lcForm.Grid1.Column12.ControlSource = UPPER("rtable.naim") &&c

		&& Header
		lcForm.Grid1.Column1.Header1.Caption = "Пр.изм." && Пр.
		lcForm.Grid1.Column2.Header1.Caption = "№ изделия"
		lcForm.Grid1.Column3.Header1.Caption = "№ изд.(ст.)"
		lcForm.Grid1.Column4.Header1.Caption = "Мод"
		lcForm.Grid1.Column5.Header1.Caption = "Код изд.(внешний)"
		lcForm.Grid1.Column6.Header1.Caption = "Код изд.(внутренний)"
		lcForm.Grid1.Column7.Header1.Caption = "Дата ввода"
		lcForm.Grid1.Column8.Header1.Caption = "Цех"
		lcForm.Grid1.Column9.Header1.Caption = "Ранг"
		lcForm.Grid1.Column10.Header1.Caption = "Код"
		lcForm.Grid1.Column11.Header1.Caption = "Пр." && Пр.изм.
		lcForm.Grid1.Column12.Header1.Caption = "Наименование"
		
		&& Visible
		FOR i = 13 TO lnResCnt&&upd
			lcForm.Grid1.Columns(i).Visible = .f.
		ENDFOR
		
		&& Alignment
		lcForm.Grid1.Column1.Alignment = 2
		lcForm.Grid1.Column2.Alignment = 2
		lcForm.Grid1.Column3.Alignment = 2
		lcForm.Grid1.Column4.Alignment = 2
		lcForm.Grid1.Column5.Alignment = 2
		lcForm.Grid1.Column6.Alignment = 2&&2
		lcForm.Grid1.Column7.Alignment = 0
		lcForm.Grid1.Column8.Alignment = 2
		lcForm.Grid1.Column9.Alignment = 2
		lcForm.Grid1.Column10.Alignment = 2
		lcForm.Grid1.Column11.Alignment = 2
		lcForm.Grid1.Column12.Alignment = 3
		
		&& Format
		lcForm.Grid1.Column7.Format = 'DEYS'
		lcForm.Grid1.Column7.Text1.Format = 'DEYS'
		
		&& ReadOnly
		lcForm.Grid1.Column6.ReadOnly = .t.&&upd

		&&Привязка событий к колонкам грида
		lcForm.MyBindEvent(lcForm.Grid1.TabIndex)
	ENDPROC
	
	PROCEDURE FixKudar
	LOCAL lcForm
		lcForm = oModel.FindForm("CustomForm")
		SELECT ALIAS()						
		&& rtable.kudar - внешний код табличный, rtable.kuda - внутренний код табличный
		&& Column5.Text1.Value - внешний код из грида, Column6.Text1.Value - внутренний код из грида
		&& таблица => грид, внешние коды равны, внутренний может не совпадать
		LOCAL lcKudarTGrid,lcKudaTGrid
		lcKudarTGrid = icod(ALLTRIM(lcForm.Grid1.Column5.Text1.Value)) && grid внешний -> внутренний
		lcKudaTGrid = ocod(lcKudarTGrid) && grid внутренний -> внешний
		&&
		IF ALLTRIM(UPPER(lcForm.Grid1.Column5.Text1.Value)) == ALLTRIM(UPPER(lcKudaTGrid))
			IF ALLTRIM(UPPER(lcForm.Grid1.Column6.Text1.Value)) != ALLTRIM(UPPER(lcKudarTGrid))
				REPLACE rtable.kuda WITH ALLTRIM(lcKudarTGrid)
				lcForm.Grid1.Column6.Text1.Value = ALLTRIM(lcKudarTGrid)
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE FixKudarAll()
	LOCAL lcForm,lnRecno
		lcForm = oModel.FindForm("CustomForm")
		SELECT "rtable"
		lnRecno = RECNO("rtable")
		&&
		SCAN
			LOCAL lcKudarTGrid,lcKudaTGrid
			lcKudarTGrid = icod(ALLTRIM(rtable.kudar)) && grid внешний -> внутренний
			lcKudaTGrid = ocod(lcKudarTGrid) && grid внутренний -> внешний
			&&
			IF ALLTRIM(UPPER(rtable.kudar)) == ALLTRIM(UPPER(lcKudaTGrid))
				IF ALLTRIM(UPPER(rtable.kuda)) != ALLTRIM(UPPER(lcKudarTGrid))
					REPLACE rtable.kuda WITH ALLTRIM(lcKudarTGrid)
				ENDIF
			ENDIF
		ENDSCAN
		GOTO (lnRecno) IN "rtable"		
	ENDPROC
	
	PROCEDURE DateTxt()&&upd
	LOCAL lcForm,lcMessageString
		&& Запись в txt		
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			SET DATE TO GERMAN
			lcMessageString = "Программа: m10870 " + ALLTRIM(TTOC(DATETIME())) + " | Файл: " + ALLTRIM(DTOC(lcForm.tdDate)) + " " + ALLTRIM(lcForm.tcTime) + " | Кол-во строк: " + ;
			ALLTRIM(STR(RECCOUNT(ALIAS()))) + CHR(13) + CHR(10)&&upd
			SET DEFAULT TO C:\FOXPRO2
			STRTOFILE(lcMessageString,"men108.txt",1)
		ENDIF
	ENDPROC
	
	PROCEDURE RepeatingRecords()&&upd
	LOCAL lcForm
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			LOCAL lnWorkArea
			lnWorkArea = oModel.IsUsed("rtable")
			IF lnWorkArea > 0
				&&Поиск повторяющихся
				LOCAL lcStringT1,lcStringT2,lcMyField
				LOCAL lnRecno,lnCnt
				lnCnt = 0
				&&
				SELECT (lnWorkArea)
				SET ORDER TO nizd
				GO TOP IN (lnWorkArea)
				&&
				DO WHILE !EOF("rtable")
					SCATTER MEMVAR
					IF ALLTRIM(m.izm) != "2"
						lcMyField = nizd
						lnRecno = RECNO("rtable")
						&&
						lcStringT1 = TRIM(m.nizd) + " " + TRIM(m.snizd) + " " + TRIM(m.mod) + " " + TRIM(m.kudar) + " " + ;
						TRIM(m.kuda) + " " + TRIM(m.cex) + " " + TRIM(m.rank) + " " + TRIM(m.kd) + " " + ;
						TRIM(m.priz) + " " + TRIM(m.naim)
						&&
						GO TOP IN (lnWorkArea)
						IF SEEK(TRIM(m.nizd),"rtable")
							SCAN WHILE UPPER(TRIM(nizd)) = UPPER(TRIM(m.nizd))
								IF lnRecno != RECNO("rtable")						
									IF ALLTRIM(izm) != "2"
										SELECT (lnWorkArea)
										lcStringT2 = TRIM(nizd) + " " + TRIM(snizd) + " " + TRIM(mod) + " " + TRIM(kudar) + " " + ;
										TRIM(kuda) + " " + TRIM(cex) + " " + TRIM(rank) + " " + TRIM(kd) + " " + ;
										TRIM(priz) + " " + TRIM(naim)
										&&
										IF UPPER(lcStringT1) == UPPER(lcStringT2)							
											REPLACE izm WITH "2"
											lnCnt = lnCnt + 1
											MESSAGEBOX(UPPER("ном.изд: " + ALLTRIM(nizd) + ", мод.: " + ALLTRIM(mod) + ", код изд.(внеш.): " + ;
											ALLTRIM(kudar) + ", код изд.(внутр.): " + ALLTRIM(kuda) + ", наим.: " + ALLTRIM(naim)),0,"Строка повторяется: №" + ALLTRIM(STR(lnCnt)))
										ENDIF						
									ENDIF
								ENDIF
							ENDSCAN
							&&
							SELECT (lnWorkArea)
							GOTO (lnRecno) IN (lnWorkArea)
						ENDIF
					ENDIF
					SKIP IN "rtable"
				ENDDO
				&& Удаление строк
				SELECT (lnWorkArea)
				DELETE FROM "rtable" WHERE ALLTRIM(izm) == "2"
			ENDIF				
		ENDIF						
	ENDPROC
	
	PROCEDURE CreateIndexFile()
	LOCAL lcTabname
		lcTabname = oPath.tcPathFile
		&&
		SET DEFAULT TO C:\FOXPRO2
		IF FILE(JUSTSTEM(lcTabname) + UPPER(".cdx"))
			DELETE FILE (JUSTSTEM(lcTabname) + UPPER(".cdx"))						
		ENDIF
		&&
		LOCAL lcCDXName
		lcCDXName = JUSTSTEM(lcTabname) + UPPER(".cdx")
		INDEX ON kuda TAG kd OF (lcCDXName)
		INDEX ON nizd+mod TAG nizd OF (lcCDXName)			
		INDEX ON cex+nizd+mod TAG cn OF (lcCDXName)
		INDEX ON mod TAG mod OF (lcCDXName)		
		INDEX ON cex+snizd+mod TAG cs1 OF (lcCDXName)	
		INDEX ON snizd+nizd+mod TAG snizd OF (lcCDXName)
		INDEX ON kudar+mod TAG kdr OF (lcCDXName)	
		INDEX ON naim+nizd+mod TAG naim OF (lcCDXName)
		INDEX ON kd+nizd+mod TAG kdn OF (lcCDXName)
		INDEX ON priz TAG priz OF (lcCDXName)
		REINDEX
	ENDPROC

	PROCEDURE FindIndex
	PARAMETERS tcName,tcTabname
	LOCAL lcTabname, ;
		lnTagCnt, ;
		lbFind
		&& Проверка наличия индекса.
		lcTabname = tcTabname &&oPath.tcPathFile
		lnTagCnt = TAGCOUNT(lcTabname,lcTabname)
		lbFind = .f.
							
		FOR i = 1 TO lnTagCnt
			lcTagName = TAG(lcTabname,i,lcTabname)
			IF lcTagName == UPPER(tcName)
				lbFind = .t.
				EXIT
			ENDIF
		ENDFOR
		RETURN lbFind
	ENDPROC
	
	PROCEDURE IndexCursor
	PARAMETERS tnIndex
	LOCAL lcForm, ;
		lbFind
		&& Индексы
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			SELECT "rtable"
			lbFind = .f.
			&&
			DO CASE			
				CASE tnIndex = 1
					&& kuda
					lbFind = THIS.FindIndex("kd","rtable")
					
					IF !lbFind
						INDEX ON kuda TAG kd
						REINDEX
					ENDIF
					
					SET ORDER TO kd && kd for kuda

				CASE tnIndex = 2
					&& nizd
					lbFind = THIS.FindIndex("nizd","rtable")
					
					IF !lbFind
						INDEX ON nizd+mod TAG nizd			
						REINDEX
					ENDIF																			
						
					SET ORDER TO nizd && nizd
					
				CASE tnIndex = 3
					&& cn	
					lbFind = THIS.FindIndex("cn","rtable")
					
					IF !lbFind
						INDEX ON cex+nizd+mod TAG cn
						REINDEX
					ENDIF																			
								
					SET ORDER TO cn

				CASE tnIndex = 4
					&& mod
					lbFind = THIS.FindIndex("mod","rtable")
					
					IF !lbFind
						INDEX ON mod TAG mod		
						REINDEX
					ENDIF																			
																
					SET ORDER TO mod
					
				CASE tnIndex = 5
					&& cs1
					lbFind = THIS.FindIndex("cs1","rtable")
					
					IF !lbFind
						INDEX ON cex+snizd+mod TAG cs1	
						REINDEX
					ENDIF					
											
					SET ORDER TO cs1
					
				CASE tnIndex = 6
					&& snizd
					lbFind = THIS.FindIndex("snizd","rtable")
					
					IF !lbFind
						INDEX ON snizd+nizd+mod TAG snizd			
						REINDEX
					ENDIF					
											
					SET ORDER TO snizd
					
				CASE tnIndex = 7
					&& kdr
					lbFind = THIS.FindIndex("kdr","rtable")
	
					IF !lbFind
						INDEX ON kudar+mod TAG kdr
						REINDEX
					ENDIF	
														
					SET ORDER TO kdr
					
				CASE tnIndex = 8
					&& naim
					lbFind = THIS.FindIndex("naim","rtable")
					
					IF !lbFind
						INDEX ON naim+nizd+mod TAG naim		
						REINDEX
					ENDIF					
									
					SET ORDER TO naim
					
				CASE tnIndex = 9
					SET ORDER TO
					
				CASE tnIndex = 10
					&& kdn
					lbFind = THIS.FindIndex("kdn","rtable")
					
					IF !lbFind
						INDEX ON kd+nizd+mod TAG kdn						
						REINDEX
					ENDIF		
																				
					SET ORDER TO kdn
					
				CASE tnIndex = 11
					&& priz
					lbFind = THIS.FindIndex("priz","rtable")

					IF !lbFind
						INDEX ON priz TAG priz 
						REINDEX
					ENDIF		
																				
					SET ORDER TO priz
			ENDCASE
		ENDIF
	ENDPROC
	
	PROCEDURE Search
	PARAMETERS tcChoiceSearch, tcUserQuery1
	LOCAL lcUserQuery1, lcForm
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			lcUserQuery1 = tcUserQuery1
			lcUserQuery1 = TRIM(lcUserQuery1)
			&&
			SELECT "rtable"
			oModel_m10870.IndexCursor(tcChoiceSearch)
			GO TOP IN "rtable"
			LOCAL lbSeek,lnRecnoT
			lnRecnoT = RECNO("rtable")
			lbSeek = SEEK(lcUserQuery1,"rtable")
			IF lbSeek
				lcForm.tnRecno = RECNO("rtable")
			ELSE
				lcForm.tnRecno = lnRecnoT
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE GetCurrentString
	LOCAL lcStringFind,lnWorkArea
		lnWorkArea = oModel.IsUsed("rtable")
		IF lnWorkArea > 0
			SELECT "rtable"
			lcStringFind = TRIM(rtable.izm) + " " + ;				
			TRIM(rtable.nizd) + " " + ;
			TRIM(rtable.snizd) + " " + ;
			TRIM(rtable.mod) + " " + ;
			TRIM(rtable.kudar) + " " + ;
			TRIM(rtable.kuda) + " " + ;
			TRIM(DTOC(rtable.dtv)) + " " + ;
			TRIM(rtable.cex) + " " + ;
			TRIM(rtable.rank) + " " + ;
			TRIM(rtable.kd) + " " + ;
			TRIM(rtable.priz) + " " + ;
			TRIM(rtable.naim)
			RETURN lcStringFind
		ENDIF
		RETURN SPACE(1)
	ENDPROC
	
	PROCEDURE SearchRecord
	PARAMETERS tcTxt,tnIndex
	LOCAL lcAliasname, lcForm, lcChto, ;
	lcStringAdd, lcStringFind, ;
	lnRecnoT		
		lcForm = oModel.FindForm("CustomForm")
		lnRecnoT = RECNO("rtable")
		IF VARTYPE(lcForm) == "O"
			lcAliasname = "rtable"
			lcStringAdd = tcTxt
			lbFind = .f.
			GO TOP IN (lcAliasname)
			&&
			DO CASE 										
				CASE tnIndex = 1
					&& nizd
					SELECT (lcAliasname)
					oModel_m10870.Search(2,m.nizd) && nizd
					
					IF FOUND("rtable")
						DO WHILE !EOF("rtable")
							IF UPPER(TRIM(m.nizd)) == UPPER(TRIM(rtable.nizd))
								lcStringFind = oModel_m10870.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("rtable")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "rtable"
									oModel_m10870.IndexCursor(2) && nizd
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "rtable"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 2
					&& cex+nizd+mod
					SELECT (lcAliasname)
					oModel_m10870.Search(3,m.cex+m.nizd) && cn
					
					IF FOUND("rtable")
						DO WHILE !EOF("rtable")
							IF UPPER(TRIM(m.cex)) == UPPER(TRIM(rtable.cex)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(rtable.nizd)) AND ;
							UPPER(TRIM(m.mod)) == UPPER(TRIM(rtable.mod))
								lcStringFind = oModel_m10870.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("rtable")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "rtable"
									oModel_m10870.IndexCursor(3) && cn
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "rtable"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 3
					&& kd+nizd+mod
					SELECT (lcAliasname)
					oModel_m10870.Search(10,m.kd+m.nizd) && kdn
					
					IF FOUND("rtable")
						DO WHILE !EOF("rtable")
							IF UPPER(TRIM(m.kd)) == UPPER(TRIM(rtable.kd)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(rtable.nizd))
								lcStringFind = oModel_m10870.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("rtable")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "rtable"
									oModel_m10870.IndexCursor(10) && kdm
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "rtable"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 4
					&& snizd+nizd+mod
					SELECT (lcAliasname)
					oModel_m10870.Search(6,m.snizd) && snizd
					
					IF FOUND("rtable")
						DO WHILE !EOF("rtable")
							IF UPPER(TRIM(m.snizd)) == UPPER(TRIM(rtable.snizd)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(rtable.nizd)) AND ;
							UPPER(TRIM(m.mod)) == UPPER(TRIM(rtable.mod))
								lcStringFind = oModel_m10870.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("rtable")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "rtable"
									oModel_m10870.IndexCursor(6) && snizd
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "rtable"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
				
				CASE tnIndex = 5
					&& kudar+mod
					SELECT (lcAliasname)
					oModel_m10870.Search(7,m.kudar) && kdr
					
					IF FOUND("rtable")
						DO WHILE !EOF("rtable")
							IF UPPER(TRIM(m.kudar)) == UPPER(TRIM(rtable.kudar)) AND UPPER(TRIM(m.mod)) == UPPER(TRIM(rtable.mod))
								lcStringFind = oModel_m10870.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("rtable")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "rtable"
									oModel_m10870.IndexCursor(7) && km
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "rtable"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF	
					
				CASE tnIndex = 6
					&& kuda
					SELECT (lcAliasname)
					oModel_m10870.Search(1,m.kuda) && kd m.kd
					
					IF FOUND("rtable")
						DO WHILE !EOF("rtable")
							IF UPPER(TRIM(m.kuda)) == UPPER(TRIM(rtable.kuda))
								lcStringFind = oModel_m10870.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("rtable")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "rtable"
									oModel_m10870.IndexCursor(1) &&
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "rtable"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 7
					&& naim
					SELECT (lcAliasname)
					oModel_m10870.Search(8,m.naim) && naim
					
					IF FOUND("rtable")
						DO WHILE !EOF("rtable")
							IF UPPER(TRIM(m.naim)) == UPPER(TRIM(rtable.naim))
								lcStringFind = oModel_m10870.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("rtable")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "rtable"
									oModel_m10870.IndexCursor(8) && naim
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "rtable"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 8
					&& priz
					SELECT (lcAliasname)
					oModel_m10870.Search(11,m.priz) && priz
					
					IF FOUND("rtable")
						DO WHILE !EOF("rtable")
							IF UPPER(TRIM(m.priz)) == UPPER(TRIM(rtable.priz))
								lcStringFind = oModel_m10870.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("rtable")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "rtable"
									oModel_m10870.IndexCursor(11) && priz
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF
							ENDIF
							
							SKIP IN "rtable"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 9
					&& новые строки
					SELECT "rtable"
					lcForm.Container1.Combo1.ListIndex = 9
					oModel_m10870.IndexCursor(9)
					GO BOTTOM IN "rtable"
					lcForm.tnRecno = RECNO("rtable")			
			ENDCASE
		ENDIF
	ENDPROC
	
	PROCEDURE CreateBackupTable()
	LOCAL lcForm,lcTabname,lcAliasname
		&& Создание таблицы с бэкапом данных
		lcForm = oModel.FindForm("CustomForm")
		lcAliasname = lcForm.tcBackupAlias
		lcTabname = lcForm.tcBackupTab
		&&
		SET DEFAULT TO C:\FOXPRO2
		LOCAL lnWorkArea
		lnWorkArea = oModel.UseIn(lcTabname,lcAliasname)
		&&
		SELECT 0
		CREATE TABLE (lcTabname) CODEPAGE = 866 (num n(5), idn n(5), izm c(1,0), nizd c(10,0), snizd c(10,0), mod c(3), kuda c(20), ;
		naim c(80), priz c(1,0), kudar c(20), dtv d(8), cex c(3), prizd c(1), kd c(2), rank c(1))
		&&
		INDEX ON num TAG num
		REINDEX
		USE
	ENDPROC
	
	PROCEDURE BackupData()
	PARAMETERS tcType,tcTemp
	LOCAL lcForm,lcTabname,lcAliasname, ;
	lnIdn
		&& Запись данных в таблицу
		lcForm = oModel.FindForm("CustomForm")
		lcAliasname = lcForm.tcBackupAlias
		lcTabname = lcForm.tcBackupTab	
		&&
		LOCAL lnWorkArea						
		lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)
		&&		
		IF lnWorkArea = 0
			SELECT 0
			USE (lcTabname) ALIAS (lcAliasname) EXCLUSIVE &&SHARED
			lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)
		ENDIF
			
		&& проверка последнего значения
		SELECT (lnWorkArea)
		GO BOTTOM IN (lnWorkArea)
		LOCAL lnRecno
		lnRecno = 1
		IF num > 0
			lnRecno = num
		ENDIF
		
		LOCAL lnColIndex
		lnColIndex = lcForm.tnColIndex
		lcForm.SetIdn(lcForm.GetIdn() + 1)
		lnIdn = lcForm.GetIdn()
		&&
		SELECT "rtable"
		SCATTER MEMVAR	
		&&	
		IF lnRecno != RECNO("rtable")
			&&номер строки rtable != предыдущей btable => смена строки
			&&value
			SELECT (lnWorkArea)
			
			INSERT INTO (lcAliasname) ;
			(num,idn,izm,nizd,snizd,mod,kuda,naim,priz,kudar,dtv,cex,kd,rank) ;
			VALUES (RECNO("rtable"),lnIdn,m.izm,m.nizd,m.snizd,m.mod,m.kuda,m.naim,m.priz,m.kudar,m.dtv,m.cex,m.kd,m.rank)
		ELSE
			&& остаемся на этой строке
			&&temp
			IF !EMPTY(tcTemp)&&upd
				DO CASE
					CASE lnColIndex = 1
						&&izm
						m.izm = tcTemp
						
					CASE lnColIndex = 2
						&&nizd
						m.nizd = tcTemp
						
					CASE lnColIndex = 3
						&&snizd
						m.snizd = tcTemp
						
					CASE lnColIndex = 4
						&&mod
						m.mod = tcTemp
						
					CASE lnColIndex = 5
						&&kuda
						m.kuda = tcTemp
						
					CASE lnColIndex = 6
						&&naim
						m.naim = tcTemp
						
					CASE lnColIndex = 7
						&&priz
						m.priz = tcTemp
						
					CASE lnColIndex = 8
						&&kudar
						m.kudar = tcTemp
						m.kuda = icod(tcTemp)
						
					CASE lnColIndex = 9
						&&dtv
						*m.dtv = tcTemp
						
					CASE lnColIndex = 10
						&&cex
						m.cex = tcTemp
						
					CASE lnColIndex = 11
						&&kd
						m.kd = tcTemp
						
					CASE lnColIndex = 12
						&&rank
						m.rank = tcTemp
				ENDCASE
				&&
				INSERT INTO (lcAliasname) ;
				(num,idn,izm,nizd,snizd,mod,kuda,naim,priz,kudar,dtv,cex,kd,rank) ;
				VALUES (RECNO("rtable"),lnIdn,m.izm,m.nizd,m.snizd,m.mod,m.kuda,m.naim,m.priz,m.kudar,m.dtv,m.cex,m.kd,m.rank)
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE DeleteBackup()
	LOCAL lcForm,lcTabname,lcAliasname
		&& Запись данных в таблицу
		lcForm = oModel.FindForm("CustomForm")
		lcAliasname = lcForm.tcBackupAlias
		lcTabname = lcForm.tcBackupTab
		&&
		SET DEFAULT TO C:\FOXPRO2
		LOCAL lnWorkArea,lnWorkAreaT
		lnWorkArea = oModel.UseIn(lcTabname,lcAliasname)
		lnWorkArea = oModel.IsUsed(lcTabname)
		lnWorkAreaT = oModel.IsUsed(lcAliasname)
		
		IF lnWorkArea = 0 AND lnWorkAreaT = 0
			DELETE FILE (lcTabname + ".DBF")
			DELETE FILE (lcTabname + ".CDX")
		ENDIF
	ENDPROC

	PROCEDURE RollbackData()
	LOCAL lcForm, ;
		  lcAliasname, lcAliasname2, ;
		  lcTabname, lcTabname2, ;
		  lnFormNumber, lnRecno, lnIdn, lnNum, ;
		  lnRecnoTemp		
		lcForm = oModel.FindForm("CustomForm")
		lcAliasname = lcForm.tcBackupAlias
		lcTabname = lcForm.tcBackupTab
		&&
		LOCAL lnWorkArea						
		lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)&&btable
		&&
		IF lnWorkArea = 0
			SELECT 0
			USE (lcTabname) ALIAS (lcAliasname) EXCLUSIVE&&SHARED
			lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)
		ENDIF
		&&
		IF lnWorkArea > 0
			SELECT (lnWorkArea)
			IF RECCOUNT(lnWorkArea) > 1
				GO BOTTOM IN (lnWorkArea)						
				DELETE IN (lnWorkArea)&&удаляем текущий вариант, который уже на экране?
				PACK IN (lnWorkArea)
				&&
				SELECT (lnWorkArea) &&
				GO BOTTOM IN (lnWorkArea)			
				SCATTER MEMVAR && копия последней добавленной строки
				lnRecno = num
				&&
				SELECT "rtable"
				GOTO (lnRecno) IN "rtable"
				REPLACE izm WITH TRIM(m.izm), ;
					nizd WITH TRIM(m.nizd), ;
					snizd WITH TRIM(m.snizd), ;
					mod WITH TRIM(m.mod), ;
					kuda WITH TRIM(icod(m.kudar)), ;
					naim WITH TRIM(m.naim), ;
					priz WITH TRIM(m.priz), ;
					kudar WITH TRIM(m.kudar), ;
					dtv WITH m.dtv, ;
					cex WITH TRIM(m.cex), ;
					kd WITH TRIM(m.kd), ;
					rank WITH TRIM(m.rank)
				&&
				lcForm.Grid1.Columns(1).Text1.Value = m.izm
				lcForm.Grid1.Columns(2).Text1.Value = m.nizd
				lcForm.Grid1.Columns(3).Text1.Value = m.snizd
				lcForm.Grid1.Columns(4).Text1.Value = m.mod
				lcForm.Grid1.Columns(5).Text1.Value = m.kudar
				lcForm.Grid1.Columns(6).Text1.Value = icod(m.kudar)
				lcForm.Grid1.Columns(7).Text1.Value = m.dtv
				lcForm.Grid1.Columns(8).Text1.Value = m.cex
				lcForm.Grid1.Columns(9).Text1.Value = m.rank
				lcForm.Grid1.Columns(10).Text1.Value = m.kd
				lcForm.Grid1.Columns(11).Text1.Value = m.priz
				lcForm.Grid1.Columns(12).Text1.Value = m.naim
				&&
				LOCAL lnColIndex
				lnColIndex = lcForm.tnColIndex
				lcForm.Grid1.Columns(lnColIndex).Text1.SetFocus()
			ENDIF	
		ENDIF
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_Req - модель для класса формы ReqForm
DEFINE CLASS Model_Req AS CUSTOM
	PROCEDURE Start
	LOCAL lcForm, lcTabname
		&& formcreate
		oModel_req.FormCreate()
		lcForm = oModel.FindForm("RequisitesForm")
		*lcTabname = oPath.tcPathFile
		&&
		IF VARTYPE(lcForm) == "O"
			oModel_req.ReadUpdate()
		ENDIF
	ENDPROC
	
	PROCEDURE ReadUpdate
		&& requisitesform
		oModel_req.SelectData()
	ENDPROC
	
	PROCEDURE FormCreate
	LOCAL lcForm
		lcForm = oModel.FindForm("RequisitesForm")
		IF VARTYPE(lcForm) != "O"
			oModel.FormCreate(4)
		ENDIF
	ENDPROC
	
	PROCEDURE PreSelectData
	LOCAL lcTabname, lcAliasname, lcForm, lcFormT
		&& form_req
		lcForm = oModel.FindForm("RequisitesForm")
		LOCAL lcFormT
		lcFormT = oModel.FindForm("CustomForm")
		lcTabname = oPath.tcPathFile
		lcAliasname = lcFormT.tcAlias
		&&
		LOCAL lnWorkArea
		lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)
		&&
		IF lnWorkArea > 0
			SELECT (lnWorkArea)
			DIMENSION laArray1(1)&&name
			ALINES(laArray1,oPath.tcRequisites,",")&& все необходимые реквизиты
			DIMENSION laArray2(ALEN(laArray1,1))&&value
			&&
			IF EMPTY(oPath.tcRequisitesAnswer)&& показывать реквизит?
				&& отображаем все реквизиты
				FOR i = 1 TO ALEN(laArray1,1)
					laArray2[i] = "Да"
				ENDFOR
			ELSE
				ALINES(laArray2,oPath.tcRequisitesAnswer,",")
			ENDIF
			
			DIMENSION laMyArray(ALEN(laArray1,1),2)
			FOR i = 1 TO ALEN(laArray1,1)
				laMyArray[i,1] = ALLTRIM(laArray1[i])
				laMyArray[i,2] = ALLTRIM(laArray2[i])
			ENDFOR
		ENDIF
	
		SELECT 0
		CREATE CURSOR "rqcursor" (name c(15),value c(3))
		INSERT INTO "rqcursor" FROM ARRAY laMyArray
		GO TOP IN "rqcursor"
	ENDPROC
	
	PROCEDURE SelectData
	LOCAL lcTabname, lcAliasname, lcForm, lcFormT
		&& form_req
		lcForm = oModel.FindForm("RequisitesForm")
		LOCAL lcFormT
		lcFormT = oModel.FindForm("CustomForm")
		lcTabname = oPath.tcPathFile
		lcAliasname = lcFormT.tcAlias
		&&
		LOCAL lnWorkArea
		lnWorkArea = oModel.IsUsed("rqcursor")
		IF lnWorkArea = 0
			&& не существует
			LOCAL lnWorkAreaT
			lnWorkAreaT = oModel.CompareWorkArea(lcTabname,lcAliasname)
			&&
			IF lnWorkAreaT > 0
				THIS.PreSelectData()
			ENDIF
		ENDIF
		
		lnWorkArea = oModel.IsUsed("rqcursor")
		IF lnWorkArea > 0
			SELECT (lnWorkArea)
			oModel_req.TableOptions()
			&&
			GO TOP IN "rqcursor"						
			lcForm.Caption = lcForm.tcPre
			oModel.FormsFillScreen(lcForm, 4)&&upd
			lcForm.Resize()					
			lcForm.Visible = .t.&&upd:перемещаем сюда
		ENDIF	
	ENDPROC
	
	PROCEDURE TableOptions
	LOCAL lcTabname, lcAliasname, lcForm
		lcForm = oModel.FindForm("RequisitesForm")
		lcTabname = oPath.tcPathFile
		lcAliasname = "rqcursor"
		&& Grid1
		SELECT "rqcursor"
		lcForm.Grid1.ColumnCount = -1	
		lcForm.Grid1.RecordSourceType = 1
		lcForm.Grid1.RecordSource = ""
		lcForm.Grid1.RecordSource = "rqcursor"
		SET DATE GERMAN  && DMY
		SET HOURS TO 24 &&upd
		DIMENSION laFields(1)	  
		LOCAL lnResCnt&&upd
		lnResCnt = AFIELDS(laFields)
		
		&& Columns
		lcForm.Grid1.Column1.ControlSource = UPPER("rqcursor.name") &&c
		lcForm.Grid1.Column2.ControlSource = UPPER("rqcursor.value") &&c

		&& Header
		lcForm.Grid1.Column1.Header1.Caption = "Рекв."
		lcForm.Grid1.Column2.Header1.Caption = "Знач."
		
		&& Alignment
		lcForm.Grid1.Column1.Alignment = 3
		lcForm.Grid1.Column2.Alignment = 3

		&& ReadOnly
		lcForm.Grid1.Column1.ReadOnly = .t.
		&&Привязка событий к колонкам грида
		lcForm.MyBindEvent(lcForm.Grid1.TabIndex)
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_Izm - модель для класса формы IzmForm
DEFINE CLASS Model_Izm AS CUSTOM
	PROCEDURE Start()
	LOCAL lcForm, lcTabname
		&& formcreate
		oModel_izm.FormCreate()
		lcForm = oModel.FindForm("IzmForm")
		&&
		IF VARTYPE(lcForm) == "O"
			oModel_izm.ReadUpdate()
		ENDIF
	ENDPROC
	
	PROCEDURE FormCreate()
	LOCAL lcForm
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) != "O"
			oModel.FormCreate(5)			
		ENDIF
	ENDPROC
	
	PROCEDURE ReadUpdate()
	LOCAL lcForm, ;
		lnWorkArea		
		lnWorkArea = oModel.IsUsed("rtable")
		IF lnWorkArea > 0
			SELECT 0
			SELECT izm,nizd,snizd,mod,kudar,kuda,dtv,cex,rank,kd,priz,naim ;
			FROM "rtable" WHERE !EMPTY(izm) INTO CURSOR "ccursor" READWRITE
			&&
			INDEX ON nizd+mod TAG nizd
			INDEX ON cex+nizd+mod TAG cn
			INDEX ON snizd+nizd+mod TAG snizd
			INDEX ON kudar+mod TAG kdr
			INDEX ON kuda TAG kd
			INDEX ON naim+nizd+mod TAG naim
			INDEX ON kd+nizd+mod TAG kdn
			REINDEX
			&&
			lnWorkArea = oModel.IsUsed("ccursor")
			IF lnWorkArea > 0
				SELECT (lnWorkArea)
				GO TOP IN (lnWorkArea)
				LOCAL lcPreParent
				lcPreParent = ""
				lcForm = oModel.FindForm("CustomForm")&&upd
				IF VARTYPE(lcForm) == "O"
					lcPreParent = lcForm.tcPre
				ENDIF
				
				lcForm = oModel.FindForm("IzmForm")
				lcForm.Container1.AddMyItem()
				oModel_izm.TableOptions()
				
				lcForm.Caption = "1 | " + lcPreParent + " | " + lcForm.tcPre + " | Кол-во строк: " + ALLTRIM(STR(RECCOUNT("ccursor")))&&upd								
				lcForm.Visible = .t.
				lcForm.Resize()
			ENDIF									
		ENDIF
	ENDPROC
	
	PROCEDURE TableOptions()
	LOCAL lcForm
		lcForm = oModel.FindForm("IzmForm")
		&& Grid1
		SELECT "ccursor"
		lcForm.Grid1.ColumnCount = -1	
		lcForm.Grid1.RecordSourceType = 1
		lcForm.Grid1.RecordSource = ""
		lcForm.Grid1.RecordSource = "ccursor"
		SET DATE GERMAN  && DMY
		SET HOURS TO 24 &&upd
		DIMENSION laFields(1)	  
		LOCAL lnResCnt&&upd
		lnResCnt = AFIELDS(laFields)
		
		&& Columns
		lcForm.Grid1.Column1.ControlSource = UPPER("ccursor.izm") &&c		
		lcForm.Grid1.Column2.ControlSource = UPPER("ccursor.nizd") &&c
		lcForm.Grid1.Column3.ControlSource = UPPER("ccursor.snizd") &&c
		lcForm.Grid1.Column4.ControlSource = UPPER("ccursor.mod") &&c
		lcForm.Grid1.Column5.ControlSource = UPPER("ccursor.kudar") &&c
		lcForm.Grid1.Column6.ControlSource = UPPER("ccursor.kuda") &&c
		lcForm.Grid1.Column7.ControlSource = UPPER("ccursor.dtv") &&d
		lcForm.Grid1.Column8.ControlSource = UPPER("ccursor.cex") &&c
		lcForm.Grid1.Column9.ControlSource = UPPER("ccursor.rank") &&c
		lcForm.Grid1.Column10.ControlSource = UPPER("ccursor.kd") &&c
		lcForm.Grid1.Column11.ControlSource = UPPER("ccursor.priz") &&c
		lcForm.Grid1.Column12.ControlSource = UPPER("ccursor.naim") &&c
		*lcForm.Grid1.Column13.ControlSource = UPPER("ccursor.prizd")

		&& Header
		lcForm.Grid1.Column1.Header1.Caption = "Пр.изм." && Пр.
		lcForm.Grid1.Column2.Header1.Caption = "№ изделия"
		lcForm.Grid1.Column3.Header1.Caption = "№ изд.(ст.)"
		lcForm.Grid1.Column4.Header1.Caption = "Мод"
		lcForm.Grid1.Column5.Header1.Caption = "Код изд.(внешний)"
		lcForm.Grid1.Column6.Header1.Caption = "Код изд.(внутренний)"
		lcForm.Grid1.Column7.Header1.Caption = "Дата ввода"
		lcForm.Grid1.Column8.Header1.Caption = "Цех"
		lcForm.Grid1.Column9.Header1.Caption = "Ранг"
		lcForm.Grid1.Column10.Header1.Caption = "Код"
		lcForm.Grid1.Column11.Header1.Caption = "Пр." && Пр.изм.
		lcForm.Grid1.Column12.Header1.Caption = "Наименование"
		*lcForm.Grid1.Column13.Header1.Caption = "Пр.(з)"
		
		&& Visible
		FOR i = 13 TO lnResCnt&&upd
			lcForm.Grid1.Columns(i).Visible = .f.
		ENDFOR
		
		&& Alignment
		lcForm.Grid1.Column1.Alignment = 2
		lcForm.Grid1.Column2.Alignment = 2
		lcForm.Grid1.Column3.Alignment = 2
		lcForm.Grid1.Column4.Alignment = 2
		lcForm.Grid1.Column5.Alignment = 2
		lcForm.Grid1.Column6.Alignment = 2&&2
		lcForm.Grid1.Column7.Alignment = 0
		lcForm.Grid1.Column8.Alignment = 2
		lcForm.Grid1.Column9.Alignment = 2
		lcForm.Grid1.Column10.Alignment = 2
		lcForm.Grid1.Column11.Alignment = 2
		lcForm.Grid1.Column12.Alignment = 3
		
		&& Format
		lcForm.Grid1.Column7.Format = 'DEYS'
		lcForm.Grid1.Column7.Text1.Format = 'DEYS'
		
		&& ReadOnly
		lcForm.Grid1.Column6.ReadOnly = .t.&&upd

		&&Привязка событий к колонкам грида
		lcForm.MyBindEvent(lcForm.Grid1.TabIndex)
	ENDPROC
	
	PROCEDURE FindIndex
	PARAMETERS tcName,tcTabname
	LOCAL lcTabname, ;
		lnTagCnt, ;
		lbFind
		&& Проверка наличия индекса.
		lcTabname = tcTabname &&oPath.tcPathFile
		lnTagCnt = TAGCOUNT(lcTabname,lcTabname)
		lbFind = .f.
							
		FOR i = 1 TO lnTagCnt
			lcTagName = TAG(lcTabname,i,lcTabname)
			IF lcTagName == UPPER(tcName)
				lbFind = .t.
				EXIT
			ENDIF
		ENDFOR
		RETURN lbFind
	ENDPROC
	
	PROCEDURE IndexCursor
	PARAMETERS tnIndex
	LOCAL lcForm, ;
		lbFind
		&& Индексы
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"
			SELECT "ccursor"
			lbFind = .f.
			&&
			DO CASE			
				CASE tnIndex = 1
					&& kuda
					lbFind = THIS.FindIndex("kd","ccursor")
					
					IF !lbFind
						INDEX ON kuda TAG kd
						REINDEX
					ENDIF
					
					SET ORDER TO kd && kd for kuda

				CASE tnIndex = 2
					&& nizd
					lbFind = THIS.FindIndex("nizd","ccursor")
					
					IF !lbFind
						INDEX ON nizd+mod TAG nizd			
						REINDEX
					ENDIF																			
						
					SET ORDER TO nizd && nizd
					
				CASE tnIndex = 3
					&& cn	
					lbFind = THIS.FindIndex("cn","ccursor")
					
					IF !lbFind
						INDEX ON cex+nizd+mod TAG cn
						REINDEX
					ENDIF																			
								
					SET ORDER TO cn

				CASE tnIndex = 4
					&& mod
					lbFind = THIS.FindIndex("mod","ccursor")
					
					IF !lbFind
						INDEX ON mod TAG mod		
						REINDEX
					ENDIF																			
																
					SET ORDER TO mod
					
				CASE tnIndex = 5
					&& cs1
					lbFind = THIS.FindIndex("cs1","ccursor")
					
					IF !lbFind
						INDEX ON cex+snizd+mod TAG cs1	
						REINDEX
					ENDIF					
											
					SET ORDER TO cs1
					
				CASE tnIndex = 6
					&& snizd
					lbFind = THIS.FindIndex("snizd","ccursor")
					
					IF !lbFind
						INDEX ON snizd+nizd+mod TAG snizd			
						REINDEX
					ENDIF					
											
					SET ORDER TO snizd
					
				CASE tnIndex = 7
					&& kdr
					lbFind = THIS.FindIndex("kdr","ccursor")
	
					IF !lbFind
						INDEX ON kudar+mod TAG kdr	
						REINDEX
					ENDIF	
														
					SET ORDER TO kdr
					
				CASE tnIndex = 8
					&& naim
					lbFind = THIS.FindIndex("naim","ccursor")
					
					IF !lbFind
						INDEX ON naim+nizd+mod TAG naim		
						REINDEX
					ENDIF					
									
					SET ORDER TO naim
					
				CASE tnIndex = 9
					SET ORDER TO
					
				CASE tnIndex = 10
					&& kdn
					lbFind = THIS.FindIndex("kdn","ccursor")
					
					IF !lbFind
						INDEX ON kd+nizd+mod TAG kdn						
						REINDEX
					ENDIF		
																				
					SET ORDER TO kdn
					
				CASE tnIndex = 11
					&& priz
					lbFind = THIS.FindIndex("priz","ccursor")

					IF !lbFind
						INDEX ON priz TAG priz 
						REINDEX
					ENDIF		
																				
					SET ORDER TO priz
			ENDCASE
		ENDIF
	ENDPROC
	
	PROCEDURE Search
	PARAMETERS tcChoiceSearch, tcUserQuery1
	LOCAL lcUserQuery1, lcForm
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"
			lcUserQuery1 = tcUserQuery1
			lcUserQuery1 = TRIM(lcUserQuery1)
			&&
			SELECT "ccursor"
			oModel_izm.IndexCursor(tcChoiceSearch)
			GO TOP IN "ccursor"
			LOCAL lbSeek,lnRecnoT
			lnRecnoT = RECNO("ccursor")
			lbSeek = SEEK(lcUserQuery1,"ccursor")
			IF lbSeek
				lcForm.tnRecno = RECNO("ccursor")
			ELSE
				lcForm.tnRecno = lnRecnoT
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE GetCurrentString
	LOCAL lcStringFind,lnWorkArea
		lnWorkArea = oModel.IsUsed("ccursor")
		IF lnWorkArea > 0
			SELECT "ccursor"
			lcStringFind = TRIM(ccursor.izm) + " " + ;				
			TRIM(ccursor.nizd) + " " + ;
			TRIM(ccursor.snizd) + " " + ;
			TRIM(ccursor.mod) + " " + ;
			TRIM(ccursor.kudar) + " " + ;
			TRIM(ccursor.kuda) + " " + ;
			TRIM(DTOC(ccursor.dtv)) + " " + ;
			TRIM(ccursor.cex) + " " + ;
			TRIM(ccursor.rank) + " " + ;
			TRIM(ccursor.kd) + " " + ;
			TRIM(ccursor.priz) + " " + ;
			TRIM(ccursor.naim)
			RETURN lcStringFind
		ENDIF
		RETURN SPACE(1)
	ENDPROC
	
	PROCEDURE SearchRecord
	PARAMETERS tcTxt,tnIndex
	LOCAL lcAliasname, lcForm, lcChto, ;
	lcStringAdd, lcStringFind, ;
	lnRecnoT		
		lcForm = oModel.FindForm("IzmForm")
		lnRecnoT = RECNO("ccursor")
		IF VARTYPE(lcForm) == "O"
			lcAliasname = "ccursor"
			lcStringAdd = tcTxt
			lbFind = .f.
			GO TOP IN (lcAliasname)
			&&
			DO CASE 										
				CASE tnIndex = 1
					&& nizd
					SELECT (lcAliasname)
					oModel_izm.Search(2,m.nizd) && nizd
					
					IF FOUND("ccursor")
						DO WHILE !EOF("ccursor")
							IF UPPER(TRIM(m.nizd)) == UPPER(TRIM(ccursor.nizd))
								lcStringFind = oModel_izm.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("ccursor")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "ccursor"
									oModel_izm.IndexCursor(2) && nizd
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "ccursor"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 2
					&& cex+nizd+mod
					SELECT (lcAliasname)
					oModel_izm.Search(3,m.cex+m.nizd) && cn
					
					IF FOUND("ccursor")
						DO WHILE !EOF("ccursor")
							IF UPPER(TRIM(m.cex)) == UPPER(TRIM(ccursor.cex)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(ccursor.nizd)) AND ;
							UPPER(TRIM(m.mod)) == UPPER(TRIM(ccursor.mod))
								lcStringFind = oModel_izm.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("ccursor")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "ccursor"
									oModel_izm.IndexCursor(3) && cn
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "ccursor"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 3
					&& kd+nizd+mod
					SELECT (lcAliasname)
					oModel_izm.Search(10,m.kd+m.nizd) && kdn
					
					IF FOUND("ccursor")
						DO WHILE !EOF("ccursor")
							IF UPPER(TRIM(m.kd)) == UPPER(TRIM(ccursor.kd)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(ccursor.nizd))
								lcStringFind = oModel_izm.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("ccursor")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "ccursor"
									oModel_izm.IndexCursor(10) && kdm
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "ccursor"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 4
					&& snizd+nizd+mod
					SELECT (lcAliasname)
					oModel_izm.Search(6,m.snizd) && snizd
					
					IF FOUND("ccursor")
						DO WHILE !EOF("ccursor")
							IF UPPER(TRIM(m.snizd)) == UPPER(TRIM(ccursor.snizd)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(ccursor.nizd)) AND ;
							UPPER(TRIM(m.mod)) == UPPER(TRIM(ccursor.mod))
								lcStringFind = oModel_izm.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("ccursor")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "ccursor"
									oModel_izm.IndexCursor(6) && snizd
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "ccursor"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
				
				CASE tnIndex = 5
					&& kudar+mod
					SELECT (lcAliasname)
					oModel_izm.Search(7,m.kudar) && kdr
					
					IF FOUND("ccursor")
						DO WHILE !EOF("ccursor")
							IF UPPER(TRIM(m.kudar)) == UPPER(TRIM(ccursor.kudar)) AND UPPER(TRIM(m.mod)) == UPPER(TRIM(ccursor.mod))
								lcStringFind = oModel_izm.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("ccursor")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "ccursor"
									oModel_izm.IndexCursor(7) && km
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "ccursor"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF	
					
				CASE tnIndex = 6
					&& kuda
					SELECT (lcAliasname)
					oModel_izm.Search(1,m.kuda) && kd m.kd
					
					IF FOUND("ccursor")
						DO WHILE !EOF("ccursor")
							IF UPPER(TRIM(m.kuda)) == UPPER(TRIM(ccursor.kuda))
								lcStringFind = oModel_izm.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("ccursor")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "ccursor"
									oModel_izm.IndexCursor(1) &&
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "ccursor"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 7
					&& naim
					SELECT (lcAliasname)
					oModel_izm.Search(8,m.naim) && naim
					
					IF FOUND("ccursor")
						DO WHILE !EOF("ccursor")
							IF UPPER(TRIM(m.naim)) == UPPER(TRIM(ccursor.naim))
								lcStringFind = oModel_izm.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("ccursor")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "ccursor"
									oModel_izm.IndexCursor(8) && naim
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "ccursor"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 8
					&& priz
					SELECT (lcAliasname)
					oModel_izm.Search(11,m.priz) && priz
					
					IF FOUND("ccursor")
						DO WHILE !EOF("ccursor")
							IF UPPER(TRIM(m.priz)) == UPPER(TRIM(ccursor.priz))
								lcStringFind = oModel_izm.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("ccursor")
									lcForm.Grid1.Refresh()
									lcForm.Grid1.SetFocus()
									&&
									SELECT "ccursor"
									oModel_izm.IndexCursor(11) && priz
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF
							ENDIF
							
							SKIP IN "ccursor"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 9
					&& новые строки
					SELECT "ccursor"
					lcForm.Container1.Combo1.ListIndex = 8
					oModel_izm.IndexCursor(9)
					GO BOTTOM IN "ccursor"
					lcForm.tnRecno = RECNO("ccursor")			
			ENDCASE
		ENDIF
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_Priz - модель для класса формы PrizForm
DEFINE CLASS Model_Priz AS CUSTOM
	PROCEDURE Start()
	LOCAL lcForm, lcTabname
		&& formcreate		
		oModel_priz.FormCreate()
		lcForm = oModel.FindForm("PrizForm")
		&&
		IF VARTYPE(lcForm) == "O"
			oModel_priz.ReadUpdate()
		ENDIF
	ENDPROC
	
	PROCEDURE FormCreate()
	LOCAL lcForm
		lcForm = oModel.FindForm("PrizForm")
		IF VARTYPE(lcForm) != "O"
			oModel.FormCreate(6)			
		ENDIF
	ENDPROC
	
	PROCEDURE ReadUpdate()
	LOCAL lcForm, ;
		lnWorkArea		
		lnWorkArea = oModel.IsUsed("rtable")
		IF lnWorkArea > 0
			lcTabname = oPath.tcPathFile&&upd
			SELECT 0
			SELECT DISTINCT SPACE(2) AS "fl",priz FROM (lcTabname) WHERE !EMPTY(izm) INTO CURSOR "pcursor" READWRITE
			&&
			lnWorkArea = oModel.IsUsed("pcursor")
			IF lnWorkArea > 0
				SELECT (lnWorkArea)
				GO TOP IN (lnWorkArea)								
				&&
				lcForm = oModel.FindForm("PrizForm")				
				IF EMPTY(oPath.tcPriz) OR EMPTY(oPath.tcPrizAnswer)
					&& загружаем значения
					DIMENSION laArray1(1)					
					SELECT DISTINCT priz FROM (lcTabname) INTO ARRAY laArray1
					&&	
					oPath.tcPriz = ""
					oPath.tcPrizAnswer = ""
					IF ALEN(laArray1,1) > 0
						FOR i = 1 TO ALEN(laArray1,1)
							oPath.tcPriz = oPath.tcPriz + laArray1[i] + ","
							oPath.tcPrizAnswer = oPath.tcPrizAnswer + ","
						ENDFOR					
					ENDIF				
					&& сохраняем значения
					oPath.SetMyParam(UPPER("tcPriz"),oPath.tcPriz)
					oPath.SetMyParam(UPPER("tcPrizAnswer"),oPath.tcPrizAnswer)
				ELSE
					&& не пустые
					SELECT (lnWorkArea)
					DIMENSION laArray1(1)&&name
					ALINES(laArray1,oPath.tcPriz,",")&& признаки
					DIMENSION laArray2(ALEN(laArray1,1))&&value
					ALINES(laArray2,oPath.tcPrizAnswer,",")&& значения
					
					LOCAL lnCnt
					lnCnt = 1
					SCAN 
						REPLACE fl WITH (laArray2[lnCnt])
						lcForm.Grid1.Columns(lcForm.tnColIndex).Text1.Value = laArray2[lnCnt]
						lnCnt = lnCnt + 1					
					ENDSCAN 
					GO TOP IN (lnWorkArea)
				ENDIF				 
				
				oModel_priz.TableOptions()
				oModel.FormsFillScreen(lcForm, 6)
				&&
				lnWorkArea = oModel.IsUsed("pcursor")&&upd
				IF lnWorkArea > 0
					SELECT (lnWorkArea)
					GO TOP IN (lnWorkArea)
				ENDIF
				
				lcForm.Caption = lcForm.tcPre&&upd
				lcForm.Visible = .t.
				lcForm.Resize()
			ENDIF									
		ENDIF
	ENDPROC
	
	PROCEDURE TableOptions()
	LOCAL lcForm
		lcForm = oModel.FindForm("PrizForm")
		&& Grid1
		SELECT "pcursor"
		lcForm.Grid1.ColumnCount = -1	
		lcForm.Grid1.RecordSourceType = 1
		lcForm.Grid1.RecordSource = ""
		lcForm.Grid1.RecordSource = "pcursor"
		SET DATE GERMAN  && DMY
		SET HOURS TO 24 &&upd
		DIMENSION laFields(1)	  
		LOCAL lnResCnt&&upd
		lnResCnt = AFIELDS(laFields)
		
		&& Columns
		lcForm.Grid1.Column1.ControlSource = UPPER("pcursor.fl") &&c		
		lcForm.Grid1.Column2.ControlSource = UPPER("pcursor.priz") &&c

		&& Header
		lcForm.Grid1.Column1.Header1.Caption = "Выб."
		lcForm.Grid1.Column2.Header1.Caption = "Пр."
		
		&& Alignment
		lcForm.Grid1.Column1.Alignment = 2
		lcForm.Grid1.Column2.Alignment = 2
		
		&& ReadOnly
		lcForm.Grid1.Column2.ReadOnly = .t.&&upd

		&&Привязка событий к колонкам грида
		lcForm.MyBindEvent(lcForm.Grid1.TabIndex)
	ENDPROC
	
	PROCEDURE SetFilter()
	LOCAL lcForm,lcTabname
		lcForm = oModel.FindForm("CustomForm")&&upd
		lcTabname = oPath.tcPathFile
		LOCAL lnWorkArea,lnWorkAreaT
		lnWorkArea = oModel.UseIn(lcTabname,"rtable")
		SELECT 0
		USE (lcTabname) ALIAS "sometest" EXCLUSIVE	
		lnWorkArea = oModel.IsUsed("sometest")
		lnWorkAreaT = oModel.IsUsed("pcursor")
		&&
		lcForm.Container1.Combo1.ListIndex = 8&&upd				
		SELECT sometest.izm,sometest.nizd,sometest.snizd,sometest.mod,sometest.kudar,sometest.kuda,sometest.dtv,sometest.cex,sometest.rank,;
		sometest.kd,sometest.priz,sometest.naim FROM "sometest" INNER JOIN "pcursor" ON sometest.priz = pcursor.priz WHERE !EMPTY(pcursor.fl) ORDER BY sometest.priz INTO CURSOR "rtable" READWRITE		
		lcForm.Grid1.Visible = .f.&&upd
		lnWorkArea = oModel.IsUsed("rtable")
		oModel_m10870.TableOptions()
		GO TOP IN "rtable"
		lcForm.Resize()
		lcForm.Grid1.Visible = .t.&&upd
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_ck - модель для класса формы CheckKdForm
DEFINE CLASS Model_ck AS CUSTOM
	PROCEDURE Start()
	LOCAL lcForm, lcTabname
		&& formcreate		
		oModel_ck.FormCreate()
		lcForm = oModel.FindForm("CheckKdForm")
		&&
		IF VARTYPE(lcForm) == "O"
			oModel_ck.ReadUpdate()
		ENDIF
	ENDPROC
	
	PROCEDURE FormCreate()
	LOCAL lcForm
		lcForm = oModel.FindForm("CheckKdForm")
		IF VARTYPE(lcForm) != "O"
			oModel.FormCreate(7)			
		ENDIF
	ENDPROC
	
	PROCEDURE ReadUpdate()
	LOCAL lcForm		
		lcForm = oModel.FindForm("CheckKdForm")
		LOCAL lnWorkArea
		lnWorkArea = oModel.IsUsed("rtable")
		IF lnWorkArea > 0
			LOCAL lnRecno,lcFormT&&upd			
*!*				lcFormT = oModel.FindForm("CustomForm")
*!*				lcFormT.Container1.Combo1.ListIndex = 3
*!*				oCContainerEvent.Combo1Click()
			SELECT 0
			CREATE CURSOR "kdcursor" (myrecno n(7))
			lnWorkAreaT = oModel.IsUsed("kdcursor")

			SELECT (lnWorkArea)
			lnRecno = RECNO(lnWorkArea)
			SET ORDER TO kdn
			SCAN 
				IF VAL(rank) > 0 AND VAL(rank) < 7 AND EMPTY(kd)
					INSERT INTO "kdcursor" (myrecno) VALUES(RECNO("rtable"))
				ENDIF
			ENDSCAN
			
			SELECT (lnWorkAreaT)
			GO TOP IN (lnWorkAreaT)
			&&
			SELECT (lnWorkArea)
			GOTO (lnRecno) IN (lnWorkArea)
			*SELECT 0
			*SELECT RECNO() AS "myrecno" FROM "rtable" WHERE VAL(rank) > 0 AND VAL(rank) < 7 AND EMPTY(kd) INTO CURSOR "kdcursor" READWRITE
			&&
*!*				LOCAL lnWorkAreaT
*!*				lnWorkAreaT = oModel.IsUsed("kdcursor")
*!*				SELECT (lnWorkAreaT)
*!*				BROWSE
		ENDIF
											
		lcForm.Caption = lcForm.tcPre
		lcForm.Resize()
		oModel.FormsFillScreen(lcForm, 7)
		lcForm.Visible = .t.
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_Select - модель для класса формы SelectForm
DEFINE CLASS Model_Select AS CUSTOM
	PROCEDURE Start
	LOCAL lcForm, lcTabname
		&& formcreate
		oModel_select.FormCreate()
		lcForm = oModel.FindForm("SelectForm")
		lcTabname = oPath.tcPathFile
		&&
		IF FILE(UPPER(lcTabname)) AND VARTYPE(lcForm) == "O"
			oModel_select.ReadUpdate()
		ENDIF
	ENDPROC
	
	PROCEDURE ReadUpdate
	LOCAL lcTabname, lcForm, ;
		  lnFormNumber
		oModel_select.SelectData()
		oModel_select.FormFill()
	ENDPROC
	
	PROCEDURE FormCreate
	LOCAL lcForm
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) != "O"
			oModel.FormCreate(8)
		ENDIF
	ENDPROC
	
	PROCEDURE FormFill
	LOCAL lcForm
		lcForm = oModel.FindForm("SelectForm")
		oModel.FormsFillScreen(lcForm, 8)
	ENDPROC
	
	PROCEDURE SelectData
	LOCAL lcTabname,lcForm
		&&
		lcForm = oModel.FindForm("SelectForm")
		lcTabname = oPath.tcPathFile
		&&
		LOCAL lnWorkArea
		lnWorkArea = oModel.CompareWorkArea(lcTabname,"rtable")
		IF lnWorkArea > 0
			SELECT 0
			SELECT DISTINCT SPACE(2) AS "fl",kd FROM "rtable" WHERE !EMPTY(kd) ORDER BY kd INTO CURSOR "kdtable1" READWRITE
			&&
			lnWorkArea = oModel.IsUsed("kdtable1")
			SELECT (lnWorkArea)
			
			IF EMPTY(oPath.tcSelect) OR EMPTY(oPath.tcSelectAnswer)&&upd
				&& загружаем значения
				DIMENSION laArray1(1)					
				SELECT DISTINCT kd FROM "kdtable1" INTO ARRAY laArray1
				&&	
				oPath.tcSelect = ""
				oPath.tcSelectAnswer = ""
				IF ALEN(laArray1,1) > 0
					FOR i = 1 TO ALEN(laArray1,1)
						oPath.tcSelect = oPath.tcSelect + laArray1[i] + ","
						oPath.tcSelectAnswer = oPath.tcSelectAnswer + ","
					ENDFOR					
				ENDIF				
				&& сохраняем значения
				oPath.SetMyParam(UPPER("tcSelect"),oPath.tcSelect)
				oPath.SetMyParam(UPPER("tcSelectAnswer"),oPath.tcSelectAnswer)
			ELSE
				&& не пустые
				SELECT (lnWorkArea)
				DIMENSION laArray1(1)&&name
				ALINES(laArray1,oPath.tcSelect,",")&& признаки
				DIMENSION laArray2(ALEN(laArray1,1))&&value
				ALINES(laArray2,oPath.tcSelectAnswer,",")&& значения
				
				LOCAL lnCnt
				lnCnt = 1
				SCAN 
					REPLACE fl WITH (laArray2[lnCnt])
					lcForm.Grid1.Columns(lcForm.tnColIndex).Text1.Value = laArray2[lnCnt]
					lnCnt = lnCnt + 1					
				ENDSCAN 
				GO TOP IN (lnWorkArea)
			ENDIF	
			
			oModel_select.TableOptions()
			GO TOP IN "kdtable1"
			lcForm.FormSettings(1)&&upd
			lcForm.Caption = lcForm.tcPre
		ENDIF
	ENDPROC
	
	PROCEDURE SelectData2
	LOCAL lcTabname,lcForm		
		lcForm = oModel.FindForm("SelectForm")	
		&&
		GO TOP IN "kdtable2"
		lcForm.Container2.AddMyItem()
		oModel_select.TableOptions2()
		lcForm.Resize()		
		lcForm.FormSettings(2)&&upd
		lcForm.Caption = lcForm.tcPre
	ENDPROC
	
	PROCEDURE TableOptions
	LOCAL lcAliasname, lcForm
		lcForm = oModel.FindForm("selectform")
		lcAliasname = "kdtable1"
		&& Grid1
		SELECT "kdtable1"
		lcForm.Grid1.ColumnCount = -1
		lcForm.Grid1.RecordSourceType = 1
		lcForm.Grid1.RecordSource = ""
		lcForm.Grid1.RecordSource = "kdtable1"
		SET DATE GERMAN  && DMY
		SET HOURS TO 24 &&upd
		DIMENSION laFields(1)	  
		LOCAL lnResCnt&&upd
		lnResCnt = AFIELDS(laFields)
		
		&& Columns
		lcForm.Grid1.Column1.ControlSource = UPPER("kdtable1.fl") &&c
		lcForm.Grid1.Column2.ControlSource = UPPER("kdtable1.kd") &&c

		&& Header
		lcForm.Grid1.Column1.Header1.Caption = "Выб."&&
		lcForm.Grid1.Column2.Header1.Caption = "Код"
				
		&& Alignment
		lcForm.Grid1.Column1.Alignment = 2
		lcForm.Grid1.Column2.Alignment = 2
				
		&& ReadOnly
		lcForm.Grid1.Column2.ReadOnly = .t.

		&&Привязка событий к колонкам грида
		lcForm.MyBindEvent(lcForm.Grid1.TabIndex)
	ENDPROC
	
	PROCEDURE TableOptions2
	LOCAL lcAliasname, lcForm
		lcForm = oModel.FindForm("selectform")
		lcAliasname = "kdtable2"
		&& Grid2
		SELECT "kdtable2"
		lcForm.Grid2.ColumnCount = -1
		lcForm.Grid2.RecordSourceType = 1
		lcForm.Grid2.RecordSource = ""
		lcForm.Grid2.RecordSource = "kdtable2"
		SET DATE GERMAN  && DMY
		SET HOURS TO 24 &&upd
		DIMENSION laFields(1)	  
		LOCAL lnResCnt&&upd
		lnResCnt = AFIELDS(laFields)
		
		&& Columns
		lcForm.Grid2.Column1.ControlSource = UPPER("kdtable2.izm") &&c		
		lcForm.Grid2.Column2.ControlSource = UPPER("kdtable2.nizd") &&c
		lcForm.Grid2.Column3.ControlSource = UPPER("kdtable2.snizd") &&c
		lcForm.Grid2.Column4.ControlSource = UPPER("kdtable2.mod") &&c
		lcForm.Grid2.Column5.ControlSource = UPPER("kdtable2.kudar") &&c
		lcForm.Grid2.Column6.ControlSource = UPPER("kdtable2.kuda") &&c
		lcForm.Grid2.Column7.ControlSource = UPPER("kdtable2.dtv") &&d
		lcForm.Grid2.Column8.ControlSource = UPPER("kdtable2.cex") &&c
		lcForm.Grid2.Column9.ControlSource = UPPER("kdtable2.rank") &&c
		lcForm.Grid2.Column10.ControlSource = UPPER("kdtable2.kd") &&c
		lcForm.Grid2.Column11.ControlSource = UPPER("kdtable2.priz") &&c
		lcForm.Grid2.Column12.ControlSource = UPPER("kdtable2.naim") &&c

		&& Header
		lcForm.Grid2.Column1.Header1.Caption = "Пр.изм." && Пр.
		lcForm.Grid2.Column2.Header1.Caption = "№ изделия"
		lcForm.Grid2.Column3.Header1.Caption = "№ изд.(ст.)"
		lcForm.Grid2.Column4.Header1.Caption = "Мод"
		lcForm.Grid2.Column5.Header1.Caption = "Код изд.(внешний)"
		lcForm.Grid2.Column6.Header1.Caption = "Код изд.(внутренний)"
		lcForm.Grid2.Column7.Header1.Caption = "Дата ввода"
		lcForm.Grid2.Column8.Header1.Caption = "Цех"
		lcForm.Grid2.Column9.Header1.Caption = "Ранг"
		lcForm.Grid2.Column10.Header1.Caption = "Код"
		lcForm.Grid2.Column11.Header1.Caption = "Пр." && Пр.изм.
		lcForm.Grid2.Column12.Header1.Caption = "Наименование"
		
		&& Visible
		FOR i = 13 TO lnResCnt&&upd
			lcForm.Grid2.Columns(i).Visible = .f.
		ENDFOR
		
		&& Alignment
		lcForm.Grid2.Column1.Alignment = 2
		lcForm.Grid2.Column2.Alignment = 2
		lcForm.Grid2.Column3.Alignment = 2
		lcForm.Grid2.Column4.Alignment = 2
		lcForm.Grid2.Column5.Alignment = 2
		lcForm.Grid2.Column6.Alignment = 2&&2
		lcForm.Grid2.Column7.Alignment = 0
		lcForm.Grid2.Column8.Alignment = 2
		lcForm.Grid2.Column9.Alignment = 2
		lcForm.Grid2.Column10.Alignment = 2
		lcForm.Grid2.Column11.Alignment = 2
		lcForm.Grid2.Column12.Alignment = 3
		
		&& Format
		lcForm.Grid2.Column7.Format = 'DEYS'
		lcForm.Grid2.Column7.Text1.Format = 'DEYS'
		
		&& ReadOnly
		lcForm.Grid2.Column6.ReadOnly = .t.&&upd

		&&Привязка событий к колонкам грида
		lcForm.MyBindEvent(lcForm.Grid2.TabIndex)
	ENDPROC

	PROCEDURE FindIndex
	PARAMETERS tcName,tcTabname
	LOCAL lcTabname, ;
		lnTagCnt, ;
		lbFind
		&& Проверка наличия индекса.
		lcTabname = tcTabname &&oPath.tcPathFile
		lnTagCnt = TAGCOUNT(lcTabname,lcTabname)
		lbFind = .f.
							
		FOR i = 1 TO lnTagCnt
			lcTagName = TAG(lcTabname,i,lcTabname)
			IF lcTagName == UPPER(tcName)
				lbFind = .t.
				EXIT
			ENDIF
		ENDFOR
		RETURN lbFind
	ENDPROC
	
	PROCEDURE IndexCursor
	PARAMETERS tnIndex
	LOCAL lcForm, ;
		lbFind
		&& Индексы
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			SELECT "kdtable2"
			lbFind = .f.
			&&
			DO CASE			
				CASE tnIndex = 1
					&& kuda
					lbFind = THIS.FindIndex("kd","kdtable2")
					
					IF !lbFind
						INDEX ON kuda TAG kd
						REINDEX
					ENDIF
					
					SET ORDER TO kd && kd for kuda

				CASE tnIndex = 2
					&& nizd
					lbFind = THIS.FindIndex("nizd","kdtable2")
					
					IF !lbFind
						INDEX ON nizd+mod TAG nizd			
						REINDEX
					ENDIF																			
						
					SET ORDER TO nizd && nizd
					
				CASE tnIndex = 3
					&& cn	
					lbFind = THIS.FindIndex("cn","kdtable2")
					
					IF !lbFind
						INDEX ON cex+nizd+mod TAG cn
						REINDEX
					ENDIF																			
								
					SET ORDER TO cn

				CASE tnIndex = 4
					&& mod
					lbFind = THIS.FindIndex("mod","kdtable2")
					
					IF !lbFind
						INDEX ON mod TAG mod		
						REINDEX
					ENDIF																			
																
					SET ORDER TO mod
					
				CASE tnIndex = 5
					&& cs1
					lbFind = THIS.FindIndex("cs1","kdtable2")
					
					IF !lbFind
						INDEX ON cex+snizd+mod TAG cs1	
						REINDEX
					ENDIF					
											
					SET ORDER TO cs1
					
				CASE tnIndex = 6
					&& snizd
					lbFind = THIS.FindIndex("snizd","kdtable2")
					
					IF !lbFind
						INDEX ON snizd+nizd+mod TAG snizd			
						REINDEX
					ENDIF					
											
					SET ORDER TO snizd
					
				CASE tnIndex = 7
					&& kdr
					lbFind = THIS.FindIndex("kdr","kdtable2")
	
					IF !lbFind
						INDEX ON kudar+mod TAG kdr	
						REINDEX
					ENDIF	
														
					SET ORDER TO kdr
					
				CASE tnIndex = 8
					&& naim
					lbFind = THIS.FindIndex("naim","kdtable2")
					
					IF !lbFind
						INDEX ON naim+nizd+mod TAG naim		
						REINDEX
					ENDIF					
									
					SET ORDER TO naim
					
				CASE tnIndex = 9
					SET ORDER TO
					
				CASE tnIndex = 10
					&& kdn
					lbFind = THIS.FindIndex("kdn","kdtable2")
					
					IF !lbFind
						INDEX ON kd+nizd+mod TAG kdn						
						REINDEX
					ENDIF		
																				
					SET ORDER TO kdn
					
				CASE tnIndex = 11
					&& priz
					lbFind = THIS.FindIndex("priz","kdtable2")

					IF !lbFind
						INDEX ON priz TAG priz 
						REINDEX
					ENDIF		
																				
					SET ORDER TO priz
			ENDCASE
		ENDIF
	ENDPROC
	
	PROCEDURE Search
	PARAMETERS tcChoiceSearch, tcUserQuery1
	LOCAL lcUserQuery1, lcForm
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			lcUserQuery1 = tcUserQuery1
			lcUserQuery1 = TRIM(lcUserQuery1)
			&&
			SELECT "kdtable2"
			oModel_select.IndexCursor(tcChoiceSearch)
			GO TOP IN "kdtable2"
			LOCAL lbSeek,lnRecnoT
			lnRecnoT = RECNO("kdtable2")
			lbSeek = SEEK(lcUserQuery1,"kdtable2")
			IF lbSeek
				lcForm.tnRecno = RECNO("kdtable2")
			ELSE
				lcForm.tnRecno = lnRecnoT
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE GetCurrentString
	LOCAL lcStringFind,lnWorkArea
		lnWorkArea = oModel.IsUsed("kdtable2")
		IF lnWorkArea > 0
			SELECT "kdtable2"
			lcStringFind = TRIM(kdtable2.izm) + " " + ;				
			TRIM(kdtable2.nizd) + " " + ;
			TRIM(kdtable2.snizd) + " " + ;
			TRIM(kdtable2.mod) + " " + ;
			TRIM(kdtable2.kudar) + " " + ;
			TRIM(kdtable2.kuda) + " " + ;
			TRIM(DTOC(kdtable2.dtv)) + " " + ;
			TRIM(kdtable2.cex) + " " + ;
			TRIM(kdtable2.rank) + " " + ;
			TRIM(kdtable2.kd) + " " + ;
			TRIM(kdtable2.priz) + " " + ;
			TRIM(kdtable2.naim)
			RETURN lcStringFind
		ENDIF
		RETURN SPACE(1)
	ENDPROC
	
	PROCEDURE SearchRecord
	PARAMETERS tcTxt,tnIndex
	LOCAL lcAliasname, lcForm, lcChto, ;
	lcStringAdd, lcStringFind, ;
	lnRecnoT		
		lcForm = oModel.FindForm("SelectForm")
		lnRecnoT = RECNO("kdtable2")
		IF VARTYPE(lcForm) == "O"
			lcAliasname = "kdtable2"
			lcStringAdd = tcTxt
			lbFind = .f.
			GO TOP IN (lcAliasname)
			&&
			DO CASE 										
				CASE tnIndex = 1
					&& nizd
					SELECT (lcAliasname)
					oModel_select.Search(2,m.nizd) && nizd
					
					IF FOUND("kdtable2")
						DO WHILE !EOF("kdtable2")
							IF UPPER(TRIM(m.nizd)) == UPPER(TRIM(kdtable2.nizd))
								lcStringFind = oModel_select.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("kdtable2")
									lcForm.Grid2.Refresh()
									lcForm.Grid2.SetFocus()
									&&
									SELECT "kdtable2"
									oModel_select.IndexCursor(2) && nizd
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "kdtable2"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 2
					&& cex+nizd+mod
					SELECT (lcAliasname)
					oModel_select.Search(3,m.cex+m.nizd) && cn
					
					IF FOUND("kdtable2")
						DO WHILE !EOF("kdtable2")
							IF UPPER(TRIM(m.cex)) == UPPER(TRIM(kdtable2.cex)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(kdtable2.nizd)) AND ;
							UPPER(TRIM(m.mod)) == UPPER(TRIM(kdtable2.mod))
								lcStringFind = oModel_select.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("kdtable2")
									lcForm.Grid2.Refresh()
									lcForm.Grid2.SetFocus()
									&&
									SELECT "kdtable2"
									oModel_select.IndexCursor(3) && cn
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "kdtable2"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 3
					&& kd+nizd+mod
					SELECT (lcAliasname)
					oModel_select.Search(10,m.kd+m.nizd) && kdn
					
					IF FOUND("kdtable2")
						DO WHILE !EOF("kdtable2")
							IF UPPER(TRIM(m.kd)) == UPPER(TRIM(kdtable2.kd)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(kdtable2.nizd))
								lcStringFind = oModel_select.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("kdtable2")
									lcForm.Grid2.Refresh()
									lcForm.Grid2.SetFocus()
									&&
									SELECT "kdtable2"
									oModel_select.IndexCursor(10) && kdm
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "kdtable2"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 4
					&& snizd+nizd+mod
					SELECT (lcAliasname)
					oModel_select.Search(6,m.snizd) && snizd
					
					IF FOUND("kdtable2")
						DO WHILE !EOF("kdtable2")
							IF UPPER(TRIM(m.snizd)) == UPPER(TRIM(kdtable2.snizd)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(kdtable2.nizd)) AND ;
							UPPER(TRIM(m.mod)) == UPPER(TRIM(kdtable2.mod))
								lcStringFind = oModel_select.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("kdtable2")
									lcForm.Grid2.Refresh()
									lcForm.Grid2.SetFocus()
									&&
									SELECT "kdtable2"
									oModel_select.IndexCursor(6) && snizd
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "kdtable2"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
				
				CASE tnIndex = 5
					&& kudar+mod
					SELECT (lcAliasname)
					oModel_select.Search(7,m.kudar) && kdr
					
					IF FOUND("kdtable2")
						DO WHILE !EOF("kdtable2")
							IF UPPER(TRIM(m.kudar)) == UPPER(TRIM(kdtable2.kudar)) AND UPPER(TRIM(m.mod)) == UPPER(TRIM(kdtable2.mod))
								lcStringFind = oModel_select.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("kdtable2")
									lcForm.Grid2.Refresh()
									lcForm.Grid2.SetFocus()
									&&
									SELECT "kdtable2"
									oModel_select.IndexCursor(7) && km
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "kdtable2"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF	
					
				CASE tnIndex = 6
					&& kuda
					SELECT (lcAliasname)
					oModel_select.Search(1,m.kuda) && kd m.kd
					
					IF FOUND("kdtable2")
						DO WHILE !EOF("kdtable2")
							IF UPPER(TRIM(m.kuda)) == UPPER(TRIM(kdtable2.kuda))
								lcStringFind = oModel_select.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("kdtable2")
									lcForm.Grid2.Refresh()
									lcForm.Grid2.SetFocus()
									&&
									SELECT "kdtable2"
									oModel_select.IndexCursor(1) &&
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "kdtable2"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 7
					&& naim
					SELECT (lcAliasname)
					oModel_select.Search(8,m.naim) && naim
					
					IF FOUND("kdtable2")
						DO WHILE !EOF("kdtable2")
							IF UPPER(TRIM(m.naim)) == UPPER(TRIM(kdtable2.naim))
								lcStringFind = oModel_select.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("kdtable2")
									lcForm.Grid2.Refresh()
									lcForm.Grid2.SetFocus()
									&&
									SELECT "kdtable2"
									oModel_select.IndexCursor(8) && naim
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF 
							ENDIF
							
							SKIP IN "kdtable2"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 8
					&& priz
					SELECT (lcAliasname)
					oModel_select.Search(11,m.priz) && priz
					
					IF FOUND("kdtable2")
						DO WHILE !EOF("kdtable2")
							IF UPPER(TRIM(m.priz)) == UPPER(TRIM(kdtable2.priz))
								lcStringFind = oModel_select.GetCurrentString()
								&&
								IF UPPER(lcStringFind) == UPPER(lcStringAdd)
									lcForm.tnRecno = RECNO("kdtable2")
									lcForm.Grid2.Refresh()
									lcForm.Grid2.SetFocus()
									&&
									SELECT "kdtable2"
									oModel_select.IndexCursor(11) && priz
									GOTO (lcForm.tnRecno) IN (lcAliasname)
									EXIT
								ENDIF
							ENDIF
							
							SKIP IN "kdtable2"
						ENDDO
					ELSE
						lcForm.tnRecno = lnRecnoT
					ENDIF
					
				CASE tnIndex = 9
					&& новые строки
					SELECT "kdtable2"
					lcForm.Container2.Combo1.ListIndex = 8
					oModel_select.IndexCursor(9)
					GO BOTTOM IN "kdtable2"
					lcForm.tnRecno = RECNO("kdtable2")			
			ENDCASE
		ENDIF
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_M10860 - модель для класса формы CustomForm2 (form_two)
DEFINE CLASS Model_M10860 AS CUSTOM
	PROCEDURE Start
	LOCAL lcForm, lcTabname
		&& formcreate
		oModel_m10860.FormCreate()
		lcForm = oModel.FindForm("CustomForm2")
		lcTabname = oPath.tcPathFile
		&&
		IF FILE(UPPER(lcTabname)) AND VARTYPE(lcForm) == "O"
			oModel_m10860.ReadUpdate()
		ENDIF
	ENDPROC
	
	PROCEDURE ReadUpdate
	LOCAL lcTabname, lcForm, ;
		  lnFormNumber
		&& customform
		oModel_m10860.SelectData()
		oModel_m10860.FormFill()
	ENDPROC
	
	PROCEDURE FormCreate
	LOCAL lcForm
		lcForm = oModel.FindForm("CustomForm2")
		IF VARTYPE(lcForm) != "O"
			oModel.FormCreate(9)
		ENDIF
	ENDPROC
	
	PROCEDURE FormFill
	LOCAL lcForm
		lcForm = oModel.FindForm("CustomForm2")
		oModel.FormsFillScreen(lcForm, 9)
	ENDPROC
	
	PROCEDURE SelectData
	LOCAL lcTabname, lcAliasname, lcAliasname1, lcForm, ;
	lcFormT
		&& form_one
		lcForm = oModel.FindForm("CustomForm2")
		lcTabname = oPath.tcPathFile
		lcAliasname = lcForm.tcAlias&&JUSTSTEM(oPath.tcPathFile)
		lcAliasname1 = oPath.tcVersionName&&"m22101"&&lcAliasname + ALLTRIM("1") && Алиас таблицы
		&&
		LOCAL lcPathFolder,lbFind&&upd
		lbFind = .f.
		SET DEFAULT TO C:\FOXPRO2
		DIMENSION laArray(1)
		ADIR(laArray,"*")			
		&&
		FOR i = 1 TO ALEN(laArray,1)
			FOR j = 1 TO ALEN(laArray,2)
				IF ALLTRIM(UPPER(laArray[i,1])) == ALLTRIM(UPPER(JUSTSTEM(lcTabname) + ".dbf"))
					lcForm.tdDate = laArray[i,3] &&дата последней модификации
					lcForm.tcTime = laArray[i,4] &&время
					lbFind = .t.
				ENDIF
				EXIT
			ENDFOR
			IF lbFind
				EXIT
			ENDIF
		ENDFOR
		&&
		LOCAL lnWorkArea
		lnWorkArea = oModel.UseIn(lcTabname,lcAliasname)
		&&
		SELECT 0
		USE (lcTabname) ALIAS "rtable" EXCLUSIVE
		&&
		PUBLIC laKdArray &&
		DIMENSION laKdArray(1) &&
		SELECT "rtable"
		*SET ORDER TO kdn
		SELECT DISTINCT SPACE(2) AS 'str',kd FROM (lcForm.tcAlias) INTO ARRAY laKdArray WHERE !EMPTY(kd) &&
		CREATE CURSOR "kcursor" (str c(2),kd c(2))
		INSERT INTO "kcursor" FROM ARRAY laKdArray
		oModel_req.PreSelectData()&&upd
		&&
		lcForm.Container1.AddMyItem()
		oModel_m10870.CreateBackupTable()&&upd
		oModel_m10870.TableOptions()
		*oModel_m10870.CreateBackupTable()
		THIS.IndexCursor(2)&&nizd
		GO TOP IN (lcAliasname)				
		lcForm.Visible = .t.&&upd:перемещаем сюда
		lcForm.Resize()
		lcForm.Caption = "1 | " + lcForm.tcPre + " | Изменение: " + ALLTRIM(DTOC(lcForm.tdDate)) + " " + ALLTRIM(lcForm.tcTime) + " | Кол-во строк: " + ;
		ALLTRIM(STR(RECCOUNT(lcAliasname)))&&upd
		SET MARK OF BAR 1 OF Pad1 TO .t.&&upd
		oPath.MenuAddBar()&&upd
	ENDPROC
	
	PROCEDURE TableOptions
	LOCAL lcTabname, lcAliasname, lcForm
		lcForm = oModel.FindForm("customform")
		lcTabname = oPath.tcPathFile
		lcAliasname = "rtable"
		&& Grid1
		SELECT "rtable"
		lcForm.Grid1.ColumnCount = -1	
		lcForm.Grid1.RecordSourceType = 1
		lcForm.Grid1.RecordSource = ""
		lcForm.Grid1.RecordSource = "rtable"
		SET DATE GERMAN  && DMY
		SET HOURS TO 24 &&upd
		DIMENSION laFields(1)	  
		LOCAL lnResCnt&&upd
		lnResCnt = AFIELDS(laFields)
		
		&& Columns
		lcForm.Grid1.Column1.ControlSource = UPPER("rtable.izm") &&c		
		lcForm.Grid1.Column2.ControlSource = UPPER("rtable.nizd") &&c
		lcForm.Grid1.Column3.ControlSource = UPPER("rtable.snizd") &&c
		lcForm.Grid1.Column4.ControlSource = UPPER("rtable.mod") &&c
		lcForm.Grid1.Column5.ControlSource = UPPER("rtable.kudar") &&c
		lcForm.Grid1.Column6.ControlSource = UPPER("rtable.kuda") &&c
		lcForm.Grid1.Column7.ControlSource = UPPER("rtable.dtv") &&d
		lcForm.Grid1.Column8.ControlSource = UPPER("rtable.cex") &&c
		lcForm.Grid1.Column9.ControlSource = UPPER("rtable.rank") &&c
		lcForm.Grid1.Column10.ControlSource = UPPER("rtable.kd") &&c
		lcForm.Grid1.Column11.ControlSource = UPPER("rtable.priz") &&c
		lcForm.Grid1.Column12.ControlSource = UPPER("rtable.naim") &&c

		&& Header
		lcForm.Grid1.Column1.Header1.Caption = "Пр.изм." && Пр.
		lcForm.Grid1.Column2.Header1.Caption = "№ изделия"
		lcForm.Grid1.Column3.Header1.Caption = "№ изд.(ст.)"
		lcForm.Grid1.Column4.Header1.Caption = "Мод"
		lcForm.Grid1.Column5.Header1.Caption = "Код изд.(внешний)"
		lcForm.Grid1.Column6.Header1.Caption = "Код изд.(внутренний)"
		lcForm.Grid1.Column7.Header1.Caption = "Дата ввода"
		lcForm.Grid1.Column8.Header1.Caption = "Цех"
		lcForm.Grid1.Column9.Header1.Caption = "Ранг"
		lcForm.Grid1.Column10.Header1.Caption = "Код"
		lcForm.Grid1.Column11.Header1.Caption = "Пр." && Пр.изм.
		lcForm.Grid1.Column12.Header1.Caption = "Наименование"
		
		&& Visible
		FOR i = 13 TO lnResCnt&&upd
			lcForm.Grid1.Columns(i).Visible = .f.
		ENDFOR
		
		&& Alignment
		lcForm.Grid1.Column1.Alignment = 2
		lcForm.Grid1.Column2.Alignment = 2
		lcForm.Grid1.Column3.Alignment = 2
		lcForm.Grid1.Column4.Alignment = 2
		lcForm.Grid1.Column5.Alignment = 2
		lcForm.Grid1.Column6.Alignment = 2&&2
		lcForm.Grid1.Column7.Alignment = 0
		lcForm.Grid1.Column8.Alignment = 2
		lcForm.Grid1.Column9.Alignment = 2
		lcForm.Grid1.Column10.Alignment = 2
		lcForm.Grid1.Column11.Alignment = 2
		lcForm.Grid1.Column12.Alignment = 3
		
		&& Format
		lcForm.Grid1.Column7.Format = 'DEYS'
		lcForm.Grid1.Column7.Text1.Format = 'DEYS'
		
		&& ReadOnly
		lcForm.Grid1.Column6.ReadOnly = .t.&&upd

		&&Привязка событий к колонкам грида
		lcForm.MyBindEvent(lcForm.Grid1.TabIndex)
	ENDPROC
	
*!*		PROCEDURE FixKudar
*!*		LOCAL lcForm
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			SELECT ALIAS()						
*!*			&& rtable.kudar - внешний код табличный, rtable.kuda - внутренний код табличный
*!*			&& Column5.Text1.Value - внешний код из грида, Column6.Text1.Value - внутренний код из грида
*!*			&& таблица => грид, внешние коды равны, внутренний может не совпадать
*!*			LOCAL lcKudarTGrid,lcKudaTGrid
*!*			lcKudarTGrid = icod(ALLTRIM(lcForm.Grid1.Column5.Text1.Value)) && grid внешний -> внутренний
*!*			lcKudaTGrid = ocod(lcKudarTGrid) && grid внутренний -> внешний
*!*			&&
*!*			IF ALLTRIM(UPPER(lcForm.Grid1.Column5.Text1.Value)) == ALLTRIM(UPPER(lcKudaTGrid))
*!*				IF ALLTRIM(UPPER(lcForm.Grid1.Column6.Text1.Value)) != ALLTRIM(UPPER(lcKudarTGrid))
*!*					REPLACE rtable.kuda WITH ALLTRIM(lcKudarTGrid)
*!*					lcForm.Grid1.Column6.Text1.Value = ALLTRIM(lcKudarTGrid)
*!*				ENDIF
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE FixKudarAll()
*!*		LOCAL lcForm,lnRecno
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			SELECT "rtable"
*!*			lnRecno = RECNO("rtable")
*!*			&&
*!*			SCAN
*!*				LOCAL lcKudarTGrid,lcKudaTGrid
*!*				lcKudarTGrid = icod(ALLTRIM(rtable.kudar)) && grid внешний -> внутренний
*!*				lcKudaTGrid = ocod(lcKudarTGrid) && grid внутренний -> внешний
*!*				&&
*!*				IF ALLTRIM(UPPER(rtable.kudar)) == ALLTRIM(UPPER(lcKudaTGrid))
*!*					IF ALLTRIM(UPPER(rtable.kuda)) != ALLTRIM(UPPER(lcKudarTGrid))
*!*						REPLACE rtable.kuda WITH ALLTRIM(lcKudarTGrid)
*!*					ENDIF
*!*				ENDIF
*!*			ENDSCAN
*!*			GOTO (lnRecno) IN "rtable"		
*!*		ENDPROC
*!*		
*!*		PROCEDURE DateTxt()&&upd
*!*		LOCAL lcForm,lcMessageString
*!*			&& Запись в txt		
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			IF VARTYPE(lcForm) == "O"
*!*				SET DATE TO GERMAN
*!*				lcMessageString = "Программа: m10870 " + ALLTRIM(TTOC(DATETIME())) + " | Файл: " + ALLTRIM(DTOC(lcForm.tdDate)) + " " + ALLTRIM(lcForm.tcTime) + " | Кол-во строк: " + ;
*!*				ALLTRIM(STR(RECCOUNT(ALIAS()))) + CHR(13) + CHR(10)&&upd
*!*				SET DEFAULT TO C:\FOXPRO2
*!*				STRTOFILE(lcMessageString,"men108.txt",1)
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE RepeatingRecords()&&upd
*!*		LOCAL lcForm
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			IF VARTYPE(lcForm) == "O"
*!*				LOCAL lnWorkArea
*!*				lnWorkArea = oModel.IsUsed("rtable")
*!*				IF lnWorkArea > 0
*!*					&&Поиск повторяющихся
*!*					LOCAL lcStringT1,lcStringT2,lcMyField
*!*					LOCAL lnRecno,lnCnt
*!*					lnCnt = 0
*!*					&&
*!*					SELECT (lnWorkArea)
*!*					SET ORDER TO nizd
*!*					GO TOP IN (lnWorkArea)
*!*					&&
*!*					DO WHILE !EOF("rtable")
*!*						SCATTER MEMVAR
*!*						IF ALLTRIM(m.izm) != "2"
*!*							lcMyField = nizd
*!*							lnRecno = RECNO("rtable")
*!*							&&
*!*							lcStringT1 = TRIM(m.nizd) + " " + TRIM(m.snizd) + " " + TRIM(m.mod) + " " + TRIM(m.kudar) + " " + ;
*!*							TRIM(m.kuda) + " " + TRIM(m.cex) + " " + TRIM(m.rank) + " " + TRIM(m.kd) + " " + ;
*!*							TRIM(m.priz) + " " + TRIM(m.naim)
*!*							&&
*!*							GO TOP IN (lnWorkArea)
*!*							IF SEEK(TRIM(m.nizd),"rtable")
*!*								SCAN WHILE UPPER(TRIM(nizd)) = UPPER(TRIM(m.nizd))
*!*									IF lnRecno != RECNO("rtable")						
*!*										IF ALLTRIM(izm) != "2"
*!*											SELECT (lnWorkArea)
*!*											lcStringT2 = TRIM(nizd) + " " + TRIM(snizd) + " " + TRIM(mod) + " " + TRIM(kudar) + " " + ;
*!*											TRIM(kuda) + " " + TRIM(cex) + " " + TRIM(rank) + " " + TRIM(kd) + " " + ;
*!*											TRIM(priz) + " " + TRIM(naim)
*!*											&&
*!*											IF UPPER(lcStringT1) == UPPER(lcStringT2)							
*!*												REPLACE izm WITH "2"
*!*												lnCnt = lnCnt + 1
*!*												MESSAGEBOX(UPPER("ном.изд: " + ALLTRIM(nizd) + ", мод.: " + ALLTRIM(mod) + ", код изд.(внеш.): " + ;
*!*												ALLTRIM(kudar) + ", код изд.(внутр.): " + ALLTRIM(kuda) + ", наим.: " + ALLTRIM(naim)),0,"Строка повторяется: №" + ALLTRIM(STR(lnCnt)))
*!*											ENDIF						
*!*										ENDIF
*!*									ENDIF
*!*								ENDSCAN
*!*								&&
*!*								SELECT (lnWorkArea)
*!*								GOTO (lnRecno) IN (lnWorkArea)
*!*							ENDIF
*!*						ENDIF
*!*						SKIP IN "rtable"
*!*					ENDDO
*!*					&& Удаление строк
*!*					SELECT (lnWorkArea)
*!*					DELETE FROM "rtable" WHERE ALLTRIM(izm) == "2"
*!*				ENDIF				
*!*			ENDIF						
*!*		ENDPROC
*!*		
*!*		PROCEDURE CreateIndexFile()
*!*		LOCAL lcTabname
*!*			lcTabname = oPath.tcPathFile
*!*			&&
*!*			SET DEFAULT TO C:\FOXPRO2
*!*			IF FILE(JUSTSTEM(lcTabname) + UPPER(".cdx"))
*!*				DELETE FILE (JUSTSTEM(lcTabname) + UPPER(".cdx"))						
*!*			ENDIF
*!*			&&
*!*			LOCAL lcCDXName
*!*			lcCDXName = JUSTSTEM(lcTabname) + UPPER(".cdx")
*!*			INDEX ON kuda TAG kd OF (lcCDXName)
*!*			INDEX ON nizd+mod TAG nizd OF (lcCDXName)			
*!*			INDEX ON cex+nizd+mod TAG cn OF (lcCDXName)
*!*			INDEX ON mod TAG mod OF (lcCDXName)		
*!*			INDEX ON cex+snizd+mod TAG cs1 OF (lcCDXName)	
*!*			INDEX ON snizd+nizd+mod TAG snizd OF (lcCDXName)
*!*			INDEX ON kudar+mod TAG kdr OF (lcCDXName)	
*!*			INDEX ON naim+nizd+mod TAG naim OF (lcCDXName)
*!*			INDEX ON kd+nizd+mod TAG kdn OF (lcCDXName)
*!*			INDEX ON priz TAG priz OF (lcCDXName)
*!*			REINDEX
*!*		ENDPROC

*!*		PROCEDURE FindIndex
*!*		PARAMETERS tcName,tcTabname
*!*		LOCAL lcTabname, ;
*!*			lnTagCnt, ;
*!*			lbFind
*!*			&& Проверка наличия индекса.
*!*			lcTabname = tcTabname &&oPath.tcPathFile
*!*			lnTagCnt = TAGCOUNT(lcTabname,lcTabname)
*!*			lbFind = .f.
*!*								
*!*			FOR i = 1 TO lnTagCnt
*!*				lcTagName = TAG(lcTabname,i,lcTabname)
*!*				IF lcTagName == UPPER(tcName)
*!*					lbFind = .t.
*!*					EXIT
*!*				ENDIF
*!*			ENDFOR
*!*			RETURN lbFind
*!*		ENDPROC
*!*		
*!*		PROCEDURE IndexCursor
*!*		PARAMETERS tnIndex
*!*		LOCAL lcForm, ;
*!*			lbFind
*!*			&& Индексы
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			IF VARTYPE(lcForm) == "O"
*!*				SELECT "rtable"
*!*				lbFind = .f.
*!*				&&
*!*				DO CASE			
*!*					CASE tnIndex = 1
*!*						&& kuda
*!*						lbFind = THIS.FindIndex("kd","rtable")
*!*						
*!*						IF !lbFind
*!*							INDEX ON kuda TAG kd
*!*							REINDEX
*!*						ENDIF
*!*						
*!*						SET ORDER TO kd && kd for kuda

*!*					CASE tnIndex = 2
*!*						&& nizd
*!*						lbFind = THIS.FindIndex("nizd","rtable")
*!*						
*!*						IF !lbFind
*!*							INDEX ON nizd+mod TAG nizd			
*!*							REINDEX
*!*						ENDIF																			
*!*							
*!*						SET ORDER TO nizd && nizd
*!*						
*!*					CASE tnIndex = 3
*!*						&& cn	
*!*						lbFind = THIS.FindIndex("cn","rtable")
*!*						
*!*						IF !lbFind
*!*							INDEX ON cex+nizd+mod TAG cn
*!*							REINDEX
*!*						ENDIF																			
*!*									
*!*						SET ORDER TO cn

*!*					CASE tnIndex = 4
*!*						&& mod
*!*						lbFind = THIS.FindIndex("mod","rtable")
*!*						
*!*						IF !lbFind
*!*							INDEX ON mod TAG mod		
*!*							REINDEX
*!*						ENDIF																			
*!*																	
*!*						SET ORDER TO mod
*!*						
*!*					CASE tnIndex = 5
*!*						&& cs1
*!*						lbFind = THIS.FindIndex("cs1","rtable")
*!*						
*!*						IF !lbFind
*!*							INDEX ON cex+snizd+mod TAG cs1	
*!*							REINDEX
*!*						ENDIF					
*!*												
*!*						SET ORDER TO cs1
*!*						
*!*					CASE tnIndex = 6
*!*						&& snizd
*!*						lbFind = THIS.FindIndex("snizd","rtable")
*!*						
*!*						IF !lbFind
*!*							INDEX ON snizd+nizd+mod TAG snizd			
*!*							REINDEX
*!*						ENDIF					
*!*												
*!*						SET ORDER TO snizd
*!*						
*!*					CASE tnIndex = 7
*!*						&& kdr
*!*						lbFind = THIS.FindIndex("kdr","rtable")
*!*		
*!*						IF !lbFind
*!*							INDEX ON kudar+mod TAG kdr
*!*							REINDEX
*!*						ENDIF	
*!*															
*!*						SET ORDER TO kdr
*!*						
*!*					CASE tnIndex = 8
*!*						&& naim
*!*						lbFind = THIS.FindIndex("naim","rtable")
*!*						
*!*						IF !lbFind
*!*							INDEX ON naim+nizd+mod TAG naim		
*!*							REINDEX
*!*						ENDIF					
*!*										
*!*						SET ORDER TO naim
*!*						
*!*					CASE tnIndex = 9
*!*						SET ORDER TO
*!*						
*!*					CASE tnIndex = 10
*!*						&& kdn
*!*						lbFind = THIS.FindIndex("kdn","rtable")
*!*						
*!*						IF !lbFind
*!*							INDEX ON kd+nizd+mod TAG kdn						
*!*							REINDEX
*!*						ENDIF		
*!*																					
*!*						SET ORDER TO kdn
*!*						
*!*					CASE tnIndex = 11
*!*						&& priz
*!*						lbFind = THIS.FindIndex("priz","rtable")

*!*						IF !lbFind
*!*							INDEX ON priz TAG priz 
*!*							REINDEX
*!*						ENDIF		
*!*																					
*!*						SET ORDER TO priz
*!*				ENDCASE
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE Search
*!*		PARAMETERS tcChoiceSearch, tcUserQuery1
*!*		LOCAL lcUserQuery1, lcForm
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			IF VARTYPE(lcForm) == "O"
*!*				lcUserQuery1 = tcUserQuery1
*!*				lcUserQuery1 = TRIM(lcUserQuery1)
*!*				&&
*!*				SELECT "rtable"
*!*				oModel_m10870.IndexCursor(tcChoiceSearch)
*!*				GO TOP IN "rtable"
*!*				LOCAL lbSeek,lnRecnoT
*!*				lnRecnoT = RECNO("rtable")
*!*				lbSeek = SEEK(lcUserQuery1,"rtable")
*!*				IF lbSeek
*!*					lcForm.tnRecno = RECNO("rtable")
*!*				ELSE
*!*					lcForm.tnRecno = lnRecnoT
*!*				ENDIF
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE GetCurrentString
*!*		LOCAL lcStringFind,lnWorkArea
*!*			lnWorkArea = oModel.IsUsed("rtable")
*!*			IF lnWorkArea > 0
*!*				SELECT "rtable"
*!*				lcStringFind = TRIM(rtable.izm) + " " + ;				
*!*				TRIM(rtable.nizd) + " " + ;
*!*				TRIM(rtable.snizd) + " " + ;
*!*				TRIM(rtable.mod) + " " + ;
*!*				TRIM(rtable.kudar) + " " + ;
*!*				TRIM(rtable.kuda) + " " + ;
*!*				TRIM(DTOC(rtable.dtv)) + " " + ;
*!*				TRIM(rtable.cex) + " " + ;
*!*				TRIM(rtable.rank) + " " + ;
*!*				TRIM(rtable.kd) + " " + ;
*!*				TRIM(rtable.priz) + " " + ;
*!*				TRIM(rtable.naim)
*!*				RETURN lcStringFind
*!*			ENDIF
*!*			RETURN SPACE(1)
*!*		ENDPROC
*!*		
*!*		PROCEDURE SearchRecord
*!*		PARAMETERS tcTxt,tnIndex
*!*		LOCAL lcAliasname, lcForm, lcChto, ;
*!*		lcStringAdd, lcStringFind, ;
*!*		lnRecnoT		
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			lnRecnoT = RECNO("rtable")
*!*			IF VARTYPE(lcForm) == "O"
*!*				lcAliasname = "rtable"
*!*				lcStringAdd = tcTxt
*!*				lbFind = .f.
*!*				GO TOP IN (lcAliasname)
*!*				&&
*!*				DO CASE 										
*!*					CASE tnIndex = 1
*!*						&& nizd
*!*						SELECT (lcAliasname)
*!*						oModel_m10870.Search(2,m.nizd) && nizd
*!*						
*!*						IF FOUND("rtable")
*!*							DO WHILE !EOF("rtable")
*!*								IF UPPER(TRIM(m.nizd)) == UPPER(TRIM(rtable.nizd))
*!*									lcStringFind = oModel_m10870.GetCurrentString()
*!*									&&
*!*									IF UPPER(lcStringFind) == UPPER(lcStringAdd)
*!*										lcForm.tnRecno = RECNO("rtable")
*!*										lcForm.Grid1.Refresh()
*!*										lcForm.Grid1.SetFocus()
*!*										&&
*!*										SELECT "rtable"
*!*										oModel_m10870.IndexCursor(2) && nizd
*!*										GOTO (lcForm.tnRecno) IN (lcAliasname)
*!*										EXIT
*!*									ENDIF 
*!*								ENDIF
*!*								
*!*								SKIP IN "rtable"
*!*							ENDDO
*!*						ELSE
*!*							lcForm.tnRecno = lnRecnoT
*!*						ENDIF
*!*						
*!*					CASE tnIndex = 2
*!*						&& cex+nizd+mod
*!*						SELECT (lcAliasname)
*!*						oModel_m10870.Search(3,m.cex+m.nizd) && cn
*!*						
*!*						IF FOUND("rtable")
*!*							DO WHILE !EOF("rtable")
*!*								IF UPPER(TRIM(m.cex)) == UPPER(TRIM(rtable.cex)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(rtable.nizd)) AND ;
*!*								UPPER(TRIM(m.mod)) == UPPER(TRIM(rtable.mod))
*!*									lcStringFind = oModel_m10870.GetCurrentString()
*!*									&&
*!*									IF UPPER(lcStringFind) == UPPER(lcStringAdd)
*!*										lcForm.tnRecno = RECNO("rtable")
*!*										lcForm.Grid1.Refresh()
*!*										lcForm.Grid1.SetFocus()
*!*										&&
*!*										SELECT "rtable"
*!*										oModel_m10870.IndexCursor(3) && cn
*!*										GOTO (lcForm.tnRecno) IN (lcAliasname)
*!*										EXIT
*!*									ENDIF 
*!*								ENDIF
*!*								
*!*								SKIP IN "rtable"
*!*							ENDDO
*!*						ELSE
*!*							lcForm.tnRecno = lnRecnoT
*!*						ENDIF
*!*						
*!*					CASE tnIndex = 3
*!*						&& kd+nizd+mod
*!*						SELECT (lcAliasname)
*!*						oModel_m10870.Search(10,m.kd+m.nizd) && kdn
*!*						
*!*						IF FOUND("rtable")
*!*							DO WHILE !EOF("rtable")
*!*								IF UPPER(TRIM(m.kd)) == UPPER(TRIM(rtable.kd)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(rtable.nizd))
*!*									lcStringFind = oModel_m10870.GetCurrentString()
*!*									&&
*!*									IF UPPER(lcStringFind) == UPPER(lcStringAdd)
*!*										lcForm.tnRecno = RECNO("rtable")
*!*										lcForm.Grid1.Refresh()
*!*										lcForm.Grid1.SetFocus()
*!*										&&
*!*										SELECT "rtable"
*!*										oModel_m10870.IndexCursor(10) && kdm
*!*										GOTO (lcForm.tnRecno) IN (lcAliasname)
*!*										EXIT
*!*									ENDIF 
*!*								ENDIF
*!*								
*!*								SKIP IN "rtable"
*!*							ENDDO
*!*						ELSE
*!*							lcForm.tnRecno = lnRecnoT
*!*						ENDIF
*!*						
*!*					CASE tnIndex = 4
*!*						&& snizd+nizd+mod
*!*						SELECT (lcAliasname)
*!*						oModel_m10870.Search(6,m.snizd) && snizd
*!*						
*!*						IF FOUND("rtable")
*!*							DO WHILE !EOF("rtable")
*!*								IF UPPER(TRIM(m.snizd)) == UPPER(TRIM(rtable.snizd)) AND UPPER(TRIM(m.nizd)) == UPPER(TRIM(rtable.nizd)) AND ;
*!*								UPPER(TRIM(m.mod)) == UPPER(TRIM(rtable.mod))
*!*									lcStringFind = oModel_m10870.GetCurrentString()
*!*									&&
*!*									IF UPPER(lcStringFind) == UPPER(lcStringAdd)
*!*										lcForm.tnRecno = RECNO("rtable")
*!*										lcForm.Grid1.Refresh()
*!*										lcForm.Grid1.SetFocus()
*!*										&&
*!*										SELECT "rtable"
*!*										oModel_m10870.IndexCursor(6) && snizd
*!*										GOTO (lcForm.tnRecno) IN (lcAliasname)
*!*										EXIT
*!*									ENDIF 
*!*								ENDIF
*!*								
*!*								SKIP IN "rtable"
*!*							ENDDO
*!*						ELSE
*!*							lcForm.tnRecno = lnRecnoT
*!*						ENDIF
*!*					
*!*					CASE tnIndex = 5
*!*						&& kudar+mod
*!*						SELECT (lcAliasname)
*!*						oModel_m10870.Search(7,m.kudar) && kdr
*!*						
*!*						IF FOUND("rtable")
*!*							DO WHILE !EOF("rtable")
*!*								IF UPPER(TRIM(m.kudar)) == UPPER(TRIM(rtable.kudar)) AND UPPER(TRIM(m.mod)) == UPPER(TRIM(rtable.mod))
*!*									lcStringFind = oModel_m10870.GetCurrentString()
*!*									&&
*!*									IF UPPER(lcStringFind) == UPPER(lcStringAdd)
*!*										lcForm.tnRecno = RECNO("rtable")
*!*										lcForm.Grid1.Refresh()
*!*										lcForm.Grid1.SetFocus()
*!*										&&
*!*										SELECT "rtable"
*!*										oModel_m10870.IndexCursor(7) && km
*!*										GOTO (lcForm.tnRecno) IN (lcAliasname)
*!*										EXIT
*!*									ENDIF 
*!*								ENDIF
*!*								
*!*								SKIP IN "rtable"
*!*							ENDDO
*!*						ELSE
*!*							lcForm.tnRecno = lnRecnoT
*!*						ENDIF	
*!*						
*!*					CASE tnIndex = 6
*!*						&& kuda
*!*						SELECT (lcAliasname)
*!*						oModel_m10870.Search(1,m.kuda) && kd m.kd
*!*						
*!*						IF FOUND("rtable")
*!*							DO WHILE !EOF("rtable")
*!*								IF UPPER(TRIM(m.kuda)) == UPPER(TRIM(rtable.kuda))
*!*									lcStringFind = oModel_m10870.GetCurrentString()
*!*									&&
*!*									IF UPPER(lcStringFind) == UPPER(lcStringAdd)
*!*										lcForm.tnRecno = RECNO("rtable")
*!*										lcForm.Grid1.Refresh()
*!*										lcForm.Grid1.SetFocus()
*!*										&&
*!*										SELECT "rtable"
*!*										oModel_m10870.IndexCursor(1) &&
*!*										GOTO (lcForm.tnRecno) IN (lcAliasname)
*!*										EXIT
*!*									ENDIF 
*!*								ENDIF
*!*								
*!*								SKIP IN "rtable"
*!*							ENDDO
*!*						ELSE
*!*							lcForm.tnRecno = lnRecnoT
*!*						ENDIF
*!*						
*!*					CASE tnIndex = 7
*!*						&& naim
*!*						SELECT (lcAliasname)
*!*						oModel_m10870.Search(8,m.naim) && naim
*!*						
*!*						IF FOUND("rtable")
*!*							DO WHILE !EOF("rtable")
*!*								IF UPPER(TRIM(m.naim)) == UPPER(TRIM(rtable.naim))
*!*									lcStringFind = oModel_m10870.GetCurrentString()
*!*									&&
*!*									IF UPPER(lcStringFind) == UPPER(lcStringAdd)
*!*										lcForm.tnRecno = RECNO("rtable")
*!*										lcForm.Grid1.Refresh()
*!*										lcForm.Grid1.SetFocus()
*!*										&&
*!*										SELECT "rtable"
*!*										oModel_m10870.IndexCursor(8) && naim
*!*										GOTO (lcForm.tnRecno) IN (lcAliasname)
*!*										EXIT
*!*									ENDIF 
*!*								ENDIF
*!*								
*!*								SKIP IN "rtable"
*!*							ENDDO
*!*						ELSE
*!*							lcForm.tnRecno = lnRecnoT
*!*						ENDIF
*!*						
*!*					CASE tnIndex = 8
*!*						&& priz
*!*						SELECT (lcAliasname)
*!*						oModel_m10870.Search(11,m.priz) && priz
*!*						
*!*						IF FOUND("rtable")
*!*							DO WHILE !EOF("rtable")
*!*								IF UPPER(TRIM(m.priz)) == UPPER(TRIM(rtable.priz))
*!*									lcStringFind = oModel_m10870.GetCurrentString()
*!*									&&
*!*									IF UPPER(lcStringFind) == UPPER(lcStringAdd)
*!*										lcForm.tnRecno = RECNO("rtable")
*!*										lcForm.Grid1.Refresh()
*!*										lcForm.Grid1.SetFocus()
*!*										&&
*!*										SELECT "rtable"
*!*										oModel_m10870.IndexCursor(11) && priz
*!*										GOTO (lcForm.tnRecno) IN (lcAliasname)
*!*										EXIT
*!*									ENDIF
*!*								ENDIF
*!*								
*!*								SKIP IN "rtable"
*!*							ENDDO
*!*						ELSE
*!*							lcForm.tnRecno = lnRecnoT
*!*						ENDIF
*!*						
*!*					CASE tnIndex = 9
*!*						&& новые строки
*!*						SELECT "rtable"
*!*						lcForm.Container1.Combo1.ListIndex = 9
*!*						oModel_m10870.IndexCursor(9)
*!*						GO BOTTOM IN "rtable"
*!*						lcForm.tnRecno = RECNO("rtable")			
*!*				ENDCASE
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE CreateBackupTable()
*!*		LOCAL lcForm,lcTabname,lcAliasname
*!*			&& Создание таблицы с бэкапом данных
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			lcAliasname = lcForm.tcBackupAlias
*!*			lcTabname = lcForm.tcBackupTab
*!*			&&
*!*			SET DEFAULT TO C:\FOXPRO2
*!*			LOCAL lnWorkArea
*!*			lnWorkArea = oModel.UseIn(lcTabname,lcAliasname)
*!*			&&
*!*			SELECT 0
*!*			CREATE TABLE (lcTabname) CODEPAGE = 866 (num n(5), idn n(5), izm c(1,0), nizd c(10,0), snizd c(10,0), mod c(3), kuda c(20), ;
*!*			naim c(80), priz c(1,0), kudar c(20), dtv d(8), cex c(3), prizd c(1), kd c(2), rank c(1))
*!*			&&
*!*			INDEX ON num TAG num
*!*			REINDEX
*!*			USE
*!*		ENDPROC
*!*		
*!*		PROCEDURE BackupData()
*!*		PARAMETERS tcType,tcTemp
*!*		LOCAL lcForm,lcTabname,lcAliasname, ;
*!*		lnIdn
*!*			&& Запись данных в таблицу
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			lcAliasname = lcForm.tcBackupAlias
*!*			lcTabname = lcForm.tcBackupTab	
*!*			&&
*!*			LOCAL lnWorkArea						
*!*			lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)
*!*			&&		
*!*			IF lnWorkArea = 0
*!*				SELECT 0
*!*				USE (lcTabname) ALIAS (lcAliasname) EXCLUSIVE &&SHARED
*!*				lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)
*!*			ENDIF
*!*				
*!*			&& проверка последнего значения
*!*			SELECT (lnWorkArea)
*!*			GO BOTTOM IN (lnWorkArea)
*!*			LOCAL lnRecno
*!*			lnRecno = 1
*!*			IF num > 0
*!*				lnRecno = num
*!*			ENDIF
*!*			
*!*			LOCAL lnColIndex
*!*			lnColIndex = lcForm.tnColIndex
*!*			lcForm.SetIdn(lcForm.GetIdn() + 1)
*!*			lnIdn = lcForm.GetIdn()
*!*			&&
*!*			SELECT "rtable"
*!*			SCATTER MEMVAR	
*!*			&&	
*!*			IF lnRecno != RECNO("rtable")
*!*				&&номер строки rtable != предыдущей btable => смена строки
*!*				&&value
*!*				SELECT (lnWorkArea)
*!*				
*!*				INSERT INTO (lcAliasname) ;
*!*				(num,idn,izm,nizd,snizd,mod,kuda,naim,priz,kudar,dtv,cex,kd,rank) ;
*!*				VALUES (RECNO("rtable"),lnIdn,m.izm,m.nizd,m.snizd,m.mod,m.kuda,m.naim,m.priz,m.kudar,m.dtv,m.cex,m.kd,m.rank)
*!*			ELSE
*!*				&& остаемся на этой строке
*!*				&&temp
*!*				IF !EMPTY(tcTemp)&&upd
*!*					DO CASE
*!*						CASE lnColIndex = 1
*!*							&&izm
*!*							m.izm = tcTemp
*!*							
*!*						CASE lnColIndex = 2
*!*							&&nizd
*!*							m.nizd = tcTemp
*!*							
*!*						CASE lnColIndex = 3
*!*							&&snizd
*!*							m.snizd = tcTemp
*!*							
*!*						CASE lnColIndex = 4
*!*							&&mod
*!*							m.mod = tcTemp
*!*							
*!*						CASE lnColIndex = 5
*!*							&&kuda
*!*							m.kuda = tcTemp
*!*							
*!*						CASE lnColIndex = 6
*!*							&&naim
*!*							m.naim = tcTemp
*!*							
*!*						CASE lnColIndex = 7
*!*							&&priz
*!*							m.priz = tcTemp
*!*							
*!*						CASE lnColIndex = 8
*!*							&&kudar
*!*							m.kudar = tcTemp
*!*							m.kuda = icod(tcTemp)
*!*							
*!*						CASE lnColIndex = 9
*!*							&&dtv
*!*							*m.dtv = tcTemp
*!*							
*!*						CASE lnColIndex = 10
*!*							&&cex
*!*							m.cex = tcTemp
*!*							
*!*						CASE lnColIndex = 11
*!*							&&kd
*!*							m.kd = tcTemp
*!*							
*!*						CASE lnColIndex = 12
*!*							&&rank
*!*							m.rank = tcTemp
*!*					ENDCASE
*!*					&&
*!*					INSERT INTO (lcAliasname) ;
*!*					(num,idn,izm,nizd,snizd,mod,kuda,naim,priz,kudar,dtv,cex,kd,rank) ;
*!*					VALUES (RECNO("rtable"),lnIdn,m.izm,m.nizd,m.snizd,m.mod,m.kuda,m.naim,m.priz,m.kudar,m.dtv,m.cex,m.kd,m.rank)
*!*				ENDIF
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE DeleteBackup()
*!*		LOCAL lcForm,lcTabname,lcAliasname
*!*			&& Запись данных в таблицу
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			lcAliasname = lcForm.tcBackupAlias
*!*			lcTabname = lcForm.tcBackupTab
*!*			&&
*!*			SET DEFAULT TO C:\FOXPRO2
*!*			LOCAL lnWorkArea,lnWorkAreaT
*!*			lnWorkArea = oModel.UseIn(lcTabname,lcAliasname)
*!*			lnWorkArea = oModel.IsUsed(lcTabname)
*!*			lnWorkAreaT = oModel.IsUsed(lcAliasname)
*!*			
*!*			IF lnWorkArea = 0 AND lnWorkAreaT = 0
*!*				DELETE FILE (lcTabname + ".DBF")
*!*				DELETE FILE (lcTabname + ".CDX")
*!*			ENDIF
*!*		ENDPROC

*!*		PROCEDURE RollbackData()
*!*		LOCAL lcForm, ;
*!*			  lcAliasname, lcAliasname2, ;
*!*			  lcTabname, lcTabname2, ;
*!*			  lnFormNumber, lnRecno, lnIdn, lnNum, ;
*!*			  lnRecnoTemp		
*!*			lcForm = oModel.FindForm("CustomForm")
*!*			lcAliasname = lcForm.tcBackupAlias
*!*			lcTabname = lcForm.tcBackupTab
*!*			&&
*!*			LOCAL lnWorkArea						
*!*			lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)&&btable
*!*			&&
*!*			IF lnWorkArea = 0
*!*				SELECT 0
*!*				USE (lcTabname) ALIAS (lcAliasname) EXCLUSIVE&&SHARED
*!*				lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)
*!*			ENDIF
*!*			&&
*!*			IF lnWorkArea > 0
*!*				SELECT (lnWorkArea)
*!*				IF RECCOUNT(lnWorkArea) > 1
*!*					GO BOTTOM IN (lnWorkArea)						
*!*					DELETE IN (lnWorkArea)&&удаляем текущий вариант, который уже на экране?
*!*					PACK IN (lnWorkArea)
*!*					&&
*!*					SELECT (lnWorkArea) &&
*!*					GO BOTTOM IN (lnWorkArea)			
*!*					SCATTER MEMVAR && копия последней добавленной строки
*!*					lnRecno = num
*!*					&&
*!*					SELECT "rtable"
*!*					GOTO (lnRecno) IN "rtable"
*!*					REPLACE izm WITH TRIM(m.izm), ;
*!*						nizd WITH TRIM(m.nizd), ;
*!*						snizd WITH TRIM(m.snizd), ;
*!*						mod WITH TRIM(m.mod), ;
*!*						kuda WITH TRIM(icod(m.kudar)), ;
*!*						naim WITH TRIM(m.naim), ;
*!*						priz WITH TRIM(m.priz), ;
*!*						kudar WITH TRIM(m.kudar), ;
*!*						dtv WITH m.dtv, ;
*!*						cex WITH TRIM(m.cex), ;
*!*						kd WITH TRIM(m.kd), ;
*!*						rank WITH TRIM(m.rank)
*!*					&&
*!*					lcForm.Grid1.Columns(1).Text1.Value = m.izm
*!*					lcForm.Grid1.Columns(2).Text1.Value = m.nizd
*!*					lcForm.Grid1.Columns(3).Text1.Value = m.snizd
*!*					lcForm.Grid1.Columns(4).Text1.Value = m.mod
*!*					lcForm.Grid1.Columns(5).Text1.Value = m.kudar
*!*					lcForm.Grid1.Columns(6).Text1.Value = icod(m.kudar)
*!*					lcForm.Grid1.Columns(7).Text1.Value = m.dtv
*!*					lcForm.Grid1.Columns(8).Text1.Value = m.cex
*!*					lcForm.Grid1.Columns(9).Text1.Value = m.rank
*!*					lcForm.Grid1.Columns(10).Text1.Value = m.kd
*!*					lcForm.Grid1.Columns(11).Text1.Value = m.priz
*!*					lcForm.Grid1.Columns(12).Text1.Value = m.naim
*!*					&&
*!*					LOCAL lnColIndex
*!*					lnColIndex = lcForm.tnColIndex
*!*					lcForm.Grid1.Columns(lnColIndex).Text1.SetFocus()
*!*				ENDIF	
*!*			ENDIF
*!*		ENDPROC
ENDDEFINE


&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Model_Help - модель для класса формы HelpForm
*!*	DEFINE CLASS Model_Help AS CUSTOM
*!*		PROCEDURE Start
*!*		LOCAL lcForm
*!*			&& Создание формы
*!*			lcForm = oModel.FindForm("HelpForm")
*!*			&&
*!*			IF VARTYPE(lcForm) != "O"
*!*				oModel.FormCreate(6) && helpform
*!*				lcForm = oModel.FindForm("HelpForm")
*!*				oModel.FormsFillScreen(lcForm, 6)
*!*				&&
*!*				oPath.SetCustom()
*!*				lcForm.Resize()
*!*				oModel.FormMove(lcForm)
*!*				lcForm.Caption = lcForm.tcPre
*!*				lcForm.Visible = .t.
*!*			ENDIF
*!*		ENDPROC
*!*	ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: Cell - класс с методами контекстного меню
DEFINE CLASS Cell AS CUSTOM
	tcValue = ""&&upd
	
	PROCEDURE SetValue()
	PARAMETERS tcValue
		THIS.tcValue = tcValue
	ENDPROC 
	
	PROCEDURE GetValue()
		RETURN THIS.tcValue
	ENDPROC 
	
	PROCEDURE SelectTable
	LOCAL lcForm, lcName
		&& Выбрать нужную таблицу
		lcName = _SCREEN.ActiveForm
		lcForm = oModel.FindForm(lcName.Class)
		RETURN lcForm	
	ENDPROC
				
	PROCEDURE GridCopyOption()
	LOCAL lcForm,lcFormT,lcTemp,lcSymbol, ;
	lnColIndex,lnSelStart,lnSelLength
		lcForm = THIS.SelectTable()
		lcFormT = oModel.MyObject()
		lnColIndex = lcForm.tnColIndex
		lnSelStart = lcFormT.Columns(lnColIndex).Text1.SelStart
		lnSelLength = lcFormT.Columns(lnColIndex).Text1.SelLength
		lcSymbol = lcFormT.Columns(lnColIndex).Text1.SelText&&char
		&&function(lcFormT.Columns(lnColIndex).Text1.Value)
		&&
		IF lcFormT.Columns(lnColIndex).Text1.SelLength > 0
			&& есть выделенная часть, замена того, что есть, на то, что вводим		
			lcTemp = SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
			ALLTRIM(lcSymbol) + ;
			SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, lnSelStart + lnSelLength + 1)
			_cliptext = lcSymbol										
		ELSE
			&& обычное нажатие и вставка
			lcTemp = SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
			ALLTRIM(lcSymbol) + ;
			SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, lnSelStart + 1)
			lcFormT.Columns(lnColIndex).Text1.SelStart = 0&&LEN(ALLTRIM(THIS.Columns(lnColIndex).Text1.Value))&&upd
			lcFormT.Columns(lnColIndex).Text1.SelLength = LEN(ALLTRIM(lcFormT.Columns(lnColIndex).Text1.Value))	
			_cliptext = lcFormT.Columns(lnColIndex).Text1.SelText				
		ENDIF
		THIS.SetValue(lcTemp)&&THIS.SetTemp(lcTemp)&&upd
	ENDPROC
	
	PROCEDURE GridPasteOption()
	LOCAL lcForm,lcFormT,lcTemp,lcSymbol, ;
	lnColIndex,lnSelStart,lnSelLength
		lcForm = THIS.SelectTable()
		lcFormT = oModel.MyObject()
		lnColIndex = lcForm.tnColIndex
		lnSelStart = lcFormT.Columns(lnColIndex).Text1.SelStart
		lnSelLength = lcFormT.Columns(lnColIndex).Text1.SelLength		
		lcSymbol = _cliptext
		&&
		IF lcFormT.Columns(lnColIndex).Text1.SelLength > 0
			&& есть выделенная часть, замена того, что есть, на то, что вводим						
			lcTemp = SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
			ALLTRIM(lcSymbol) + ;
			SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, lnSelStart + lnSelLength + 1)
			lnMySelLength = lnSelStart + LEN(ALLTRIM(_cliptext))&&
		ELSE
			&& обычное нажатие и вставка
			lcTemp = SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
			ALLTRIM(lcSymbol) + ;
			SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, lnSelStart + 1)
			lnMySelLength = lnSelStart + LEN(ALLTRIM(_cliptext))
		ENDIF				
		&&
		lcFormT.Columns(lnColIndex).Text1.Value = lcTemp
		lcFormT.Columns(lnColIndex).Text1.SelStart = lnMySelLength
		THIS.SetValue(lcTemp)&&upd
	ENDPROC
	
	PROCEDURE GridCutOption()
	LOCAL lcForm,lcFormT,lcTemp,lcSymbol, ;
	lnColIndex,lnSelStart,lnSelLength
		lcForm = THIS.SelectTable()
		lcFormT = oModel.MyObject()
		lnColIndex = lcForm.tnColIndex
		lnSelStart = lcFormT.Columns(lnColIndex).Text1.SelStart
		lnSelLength = lcFormT.Columns(lnColIndex).Text1.SelLength
		lcSymbol = lcFormT.Columns(lnColIndex).Text1.SelText
		&&	
		IF lcFormT.Columns(lnColIndex).Text1.SelLength > 0
			&& есть выделенная часть, замена того, что есть, на то, что вводим
			lcTemp = SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
			SUBSTR(lcFormT.Columns(lnColIndex).Text1.Value, lnSelStart + lnSelLength + 1)
			_cliptext = lcSymbol
		ELSE
			&& обычное нажатие и вставка
		ENDIF
		lcFormT.Columns(lnColIndex).Text1.Value = lcTemp
		lcFormT.Columns(lnColIndex).Text1.SelStart = lnSelStart
		THIS.SetValue(lcTemp)&&upd
	ENDPROC
	
	PROCEDURE GridSelectAll()
	LOCAL lcForm,lcFormT, ;
	lnColIndex
		lcForm = THIS.SelectTable()
		lcFormT = oModel.MyObject()
		lnColIndex = lcForm.tnColIndex
		&& ctrl+a
		lcFormT.Columns(lnColIndex).Text1.SelStart = 0
		lcFormT.Columns(lnColIndex).Text1.SelLength = LEN(ALLTRIM(lcFormT.Columns(lnColIndex).Text1.Value))
	ENDPROC
	
	PROCEDURE TextCopyOption()
	LOCAL lcForm, lcAliasname, lcCopy, lcActive, ;
		lcFormT, ;
	 	lnRecno
		&& Копировать выбранные данные
		&& Параметры таблицы
		lcForm = THIS.SelectTable()
		lcFormT = oModel.MyObject()
		lcActive = UPPER(lcForm.ActiveControl.Name)
		&&
		IF AT(UPPER("text"),lcActive) > 0&&upd
			IF lcFormT.SelLength > 0 AND lcFormT.SelLength < LEN(ALLTRIM(lcFormT.Value))
				&& выделено
				lcFormT.SelLength = lcFormT.SelLength
			ELSE
				&& не выделено->общий случай
				lcFormT.SelStart = 0
				lcFormT.SelLength = LEN(ALLTRIM(lcFormT.Value))
			ENDIF
			
			_cliptext = lcFormT.SelText
			lcFormT.SetFocus()
		ENDIF
	ENDPROC
	
	PROCEDURE TextPasteOption()
	LOCAL lcForm, lcAliasname, lcOld1, lcOld2, ;
		  lcPaste, lcActive, lcFormT, ;
	 	  lnRecno, lnSelSCopy
		&& Вставить выбранные данные
		lcForm = THIS.SelectTable()
		lcFormT = oModel.MyObject()
		IF lcFormT.SelLength > 0
			&& Есть выделенная часть.
			lnSelSCopy = lcFormT.SelStart
			lcOld1 = SUBSTR(lcFormT.Value, 1, lcFormT.SelStart)
			lcOld2 = SUBSTR(lcFormT.Value, lcFormT.SelStart + lcFormT.SelLength + 1)
			&&
			lcFormT.Value = lcOld1 + _cliptext + ALLTRIM(lcOld2)
			lcFormT.SetFocus()
			lcFormT.SelStart = lnSelSCopy + LEN(ALLTRIM(_cliptext))&&
		ELSE
			&& Нет выделенной части.
			lnSelSCopy = lcFormT.SelStart
			lcOld1 = SUBSTR(lcFormT.Value, 1, lcFormT.SelStart)
			lcOld2 = SUBSTR(lcFormT.Value, lcFormT.SelStart + 1)
			&&
			lcFormT.Value = lcOld1 + _cliptext + ALLTRIM(lcOld2)
			lcFormT.SetFocus()
			lcFormT.SelStart = lnSelSCopy + LEN(ALLTRIM(_cliptext))&&
		ENDIF
	ENDPROC
	
	PROCEDURE TextCutOption()
	LOCAL lcForm,lcFormT,lcTemp,lcSymbol, ;
	lnSelStart,lnSelLength, ;
	loNode
		lcForm = THIS.SelectTable()
		lcFormT = oModel.MyObject()
		IF VARTYPE(lcForm) == "O"
			*loNode = lcForm.Container1.Text1
			lcSymbol = lcFormT.SelText
			lnSelStart = lcFormT.SelStart
			lnSelLength = lcFormT.SelLength
			&&
			IF lcFormT.SelLength > 0
				&& есть выделенная часть, замена того, что есть, на то, что вводим
				lcTemp = SUBSTR(lcFormT.Value, 1, lnSelStart) + ;
				SUBSTR(lcFormT.Value, lnSelStart + lnSelLength + 1)
				_cliptext = lcSymbol
				lcFormT.Value = lcTemp
				lcFormT.SelStart = lnSelStart
			ELSE
				&& обычное нажатие и вставка				
			ENDIF
		ENDIF		
	ENDPROC
	
	PROCEDURE TextSelectAll()
	LOCAL lcForm,lcFormT
		lcForm = THIS.SelectTable()
		lcFormT = oModel.MyObject()
		&&
		lcFormT.SelStart = 0
		lcFormT.SelLength = LEN(ALLTRIM(lcFormT.Value))
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& Class: FileForm
DEFINE CLASS FileForm AS FORM
	BackColor = RGB(219,215,210)
	BorderStyle = 1
	Height = 1
	Left = 1
	MaxButton = .f.
	ShowTips = .t.
	Top = 1	
	Visible = .t.&&f
	Width = 1
	ShowWindow = 0
	
	tcPathExe = "" && Директория, из которой произведен запуск EXE
	tcPre = "" && название таблицы
	
	PROCEDURE FormSettings()
	PARAMETERS tnOnOff
	LOCAL lcForm
		lcForm = oModel.FindForm("FileForm")
		IF tnOnOff = 1
			&& SelectData
			lcForm.Container1.Visible = .t.
			lcForm.Container2.Visible = .f.
		ELSE
			lcForm.Container1.Visible = .f.
			lcForm.Container2.Visible = .t.
		ENDIF
 	ENDPROC
	
	PROCEDURE Init()
		_SCREEN.AddObject("ScClass","ScreenClass")
		oModel.SetHotKeys()
		THIS.FormSettings(1)&&
		THIS.Resize()
	ENDPROC
	
	PROCEDURE Resize()
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER('container'),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.Resize()
			ENDIF
		ENDFOR	
		&&
		oPath.SetCustomFont()
	ENDPROC
	
	PROCEDURE QueryUnload()
		QUIT
		&& нужно сделать = возврат к главной форме
	ENDPROC
	
	&& Container1
 	ADD OBJECT Container1 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .t.
 	
 	PROCEDURE Container1.Resize()
 		&& label left
		FOR EACH myobject IN THIS.Controls
			IF ALLTRIM(UPPER(myobject.Class)) == ALLTRIM(UPPER("label"))
				myobject.Left = THIS.Label1.Height
			ENDIF
		ENDFOR
		&& line
		THIS.Line1.Left = 0
		&& label
		THIS.Label1.Top = THIS.Label1.Height&&5
		THIS.Line1.Top = THIS.Label1.Top + THIS.Label1.Height
		THIS.Label2.Top = THIS.Line1.Top + THIS.Line1.Height + 2
		THIS.Label3.Top = THIS.Label2.Top + THIS.Label2.Height + 2
		THIS.Label4.Top = THIS.Label3.Top + THIS.Label3.Height + 2
		THIS.Label5.Top = THIS.Label4.Top + THIS.Label4.Height + 2
		THIS.Label6.Top = THIS.Label5.Top + THIS.Label5.Height + 2
		
		&& form
		&& width
		THIS.Line1.Width = THISFORM.Width
		THIS.Width = THISFORM.Width
		&& height
		LOCAL lnCnt
		lnCnt = 0
		&&
		FOR EACH myobject IN THIS.Controls
			lnCnt = lnCnt + myobject.Height + 1
		ENDFOR
		&&
		THIS.Height = THIS.Label6.Top + THIS.Label6.Height + THIS.Label1.Height&&lnCnt + THIS.Label1.Top * 2
		THISFORM.Height = THIS.Height
 	ENDPROC
 	
 	PROCEDURE Container1.Init()	
		&& Label1
		THIS.AddObject("Label1","Label")
		WITH THIS.Label1
			.Alignment = 0
			.AutoSize = .t.
			.BackColor = RGB(219,215,210)
			.Caption = "Выберите задачу:"
			.Enabled = .t.
			.Height = 26
			.Visible = .t.
		ENDWITH
		
		&& Label2
		THIS.AddObject("Label2","Label")
		WITH THIS.Label2
			.Alignment = 0
			.AutoSize = .t.
			.BackColor = RGB(248,244,255)
			.Caption = "1.M10870"
			.Enabled = .t.
			.FontUnderline = .t.
			.Height = 26
			.Visible = .t.
		ENDWITH
		&&
		BINDEVENT(THIS.Label2,"Click",oMContainerEvent,"Label2Click")
		BINDEVENT(THIS.Label2,"Error",oMContainerEvent,"Label2Error")
		
		&& Label3
		THIS.AddObject("Label3","Label")
		WITH THIS.Label3
			.Alignment = 0
			.AutoSize = .t.
			.BackColor = RGB(248,244,255)
			.Caption = "2.M10860"
			.Enabled = .t.
			.FontUnderline = .t.
			.Height = 26
			.Visible = .t.
		ENDWITH
		&&
		BINDEVENT(THIS.Label3,"Click",oMContainerEvent,"Label3Click")
		BINDEVENT(THIS.Label3,"Error",oMContainerEvent,"Label3Error")

		&& Label4
		THIS.AddObject("Label4","Label")
		WITH THIS.Label4
			.Alignment = 0
			.AutoSize = .t.
			.BackColor = RGB(248,244,255)
			.Caption = "3.M10880"
			.Enabled = .t.
			.FontUnderline = .t.
			.Height = 26
			.Visible = .t.
		ENDWITH
		&&
		BINDEVENT(THIS.Label4,"Click",oMContainerEvent,"Label4Click")
		BINDEVENT(THIS.Label4,"Error",oMContainerEvent,"Label4Error")

		&& Label5
		THIS.AddObject("Label5","Label")
		WITH THIS.Label5
			.Alignment = 0
			.AutoSize = .t.
			.BackColor = RGB(248,244,255)
			.Caption = "4.M10881"
			.Enabled = .t.
			.FontUnderline = .t.
			.Height = 26
			.Visible = .t.
		ENDWITH
		&&
		BINDEVENT(THIS.Label5,"Click",oMContainerEvent,"Label5Click")
		BINDEVENT(THIS.Label5,"Error",oMContainerEvent,"Label5Error")
	
		&& Label6
		THIS.AddObject("Label6","Label")
		WITH THIS.Label6
			.Alignment = 0
			.AutoSize = .t.
			.BackColor = RGB(248,244,255)
			.Caption = "5.M10881K"
			.Enabled = .t.
			.FontUnderline = .t.
			.Height = 26
			.Visible = .t.
		ENDWITH
		&&
		BINDEVENT(THIS.Label6,"Click",oMContainerEvent,"Label6Click")
		BINDEVENT(THIS.Label6,"Error",oMContainerEvent,"Label6Error")
	
		&& Line1
		THIS.AddObject("Line1","Line")
		WITH THIS.Line1
			.Height = 0
			.Left = 1
			.Top = 3
			.Visible = .t.
			.Width = 1
		ENDWITH
	ENDPROC
 	
 	&& Container2
 	ADD OBJECT Container2 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .t.
 	
 	PROCEDURE Container2.Resize()
 		THIS.Height = THISFORM.Height
		THIS.Width = THISFORM.Width
		&& 	
		THIS.Label3.Left = THIS.Label3.Height&&1
		THIS.Label3.Top = THIS.Label3.Height&&5
		&& line1
		THIS.Line1.Left = 0
		THIS.Line1.Top = THIS.Label3.Top + THIS.Label3.Height &&+ 2
		&& text2
		THIS.Text2.Left = 1
		THIS.Text2.Top = THIS.Line1.Top + THIS.Line1.Height
		&& text1
		THIS.Text1.Left = THIS.Text2.Left
		THIS.Text1.Top = THIS.Text2.Top + THIS.Text2.Height - 1										
		&& command1 command2
		THIS.Command1.Top = THIS.Text1.Top + THIS.Text1.Height * 2 + 1
		THIS.Command2.Top = THIS.Command1.Top
		&& form
		THIS.Text1.Width = THISFORM.Width - 2
		THIS.Text2.Width = THIS.Text1.Width
		THIS.Command2.Left = THISFORM.Width - 1 - THIS.Command2.Width
		THIS.Command1.Left = THIS.Command2.Left - 1 - THIS.Command1.Width
		THIS.Label3.Left = (THISFORM.Width - THISFORM.Width * 0.9) / 2
		THIS.Line1.Width = THISFORM.Width
 	ENDPROC
 	
 	PROCEDURE Container2.Init()
		&& Label1
		THIS.AddObject("Label1","Label")
		WITH THIS.Label1
			.Alignment = 2
			.AutoSize = .t.
			.BackColor = RGB(248,244,255)
			.Caption = 'Путь к таблице:'
			.Enabled = .f.
			.Height = 24
			.Visible = .f.
		ENDWITH
		&&
		BINDEVENT(THIS.Label1,"Error",oFContainerEvent,"Label1Error")
		
		&& Label2
		THIS.AddObject("Label2","Label")
		WITH THIS.Label2
			.Alignment = 0
			.AutoSize = .t.
			.BackColor = RGB(255,117,117)
			.Enabled = .f.
			.Height = 26
			.Visible = .f.
		ENDWITH
		&&
		BINDEVENT(THIS.Label2,"Error",oFContainerEvent,"Label2Error")
		
		&& Label3
		THIS.AddObject("Label3","Label")
		WITH THIS.Label3
			.Alignment = 0
			.AutoSize = .t.
			.BackColor = RGB(219,215,210)
			.Caption = '< Назад'
			.Enabled = .t.
			.Height = 26
			.Visible = .t.
		ENDWITH
		&&
		BINDEVENT(THIS.Label3,"Click",oFContainerEvent,"Label3Click")
		BINDEVENT(THIS.Label3,"Error",oFContainerEvent,"Label3Error")
		
		&& Text1
		THIS.AddObject("Text1","TextBox")
		WITH THIS.Text1
			.Alignment = 0
			.BackColor = RGB(248,244,255)
			.Enabled = .t.
			.Height = 26
			.Visible = .t.
			.Width = 100
		ENDWITH
		&&
		BINDEVENT(THIS.Text1,"Error",oFContainerEvent,"Text1Error")
		
		&& Text2
		THIS.AddObject("Text2","TextBox")
		WITH THIS.Text2
			.Alignment = 0
			.BackColor = RGB(248,244,255)
			.Enabled = .t.
			.Height = 26
			.Value = "Путь к таблице:"
			.Visible = .t.
			.Width = 100
		ENDWITH
		&&
		BINDEVENT(THIS.Text2,"KeyPress",oFContainerEvent,"Text2KeyPress")
		BINDEVENT(THIS.Text2,"LostFocus",oFContainerEvent,"Text2LostFocus")
		BINDEVENT(THIS.Text2,"Error",oFContainerEvent,"Text2Error")
		
		&& Command1 
		THIS.AddObject("Command1","CommandButton")
		WITH THIS.Command1 
			.Caption = "Открыть"
			.Height = 26
			.ToolTipText = "Enter"
			.Visible = .t.
			.Width = 80
		ENDWITH
		&&upd
		LOCAL lcForm
		lcForm = oModel.FindForm("FileForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.Width = (THIS.Command1.Width + 1) * 4
		ENDIF
		&&
		BINDEVENT(THIS.Command1,"Click",oFContainerEvent,"Command1Click")
		BINDEVENT(THIS.Command1,"Error",oFContainerEvent,"Command1Error")
		
		&& Command2 
		THIS.AddObject("Command2","CommandButton")
		WITH THIS.Command2 
			.Caption = "Закрыть"
			.Height = 26
			.ToolTipText = "Esc"
			.Visible = .t.
			.Width = 80
		ENDWITH
		&&
		BINDEVENT(THIS.Command2,"Click",oFContainerEvent,"Command2Click")
		BINDEVENT(THIS.Command2,"Error",oFContainerEvent,"Command2Error")
		
		&& Line1 
		THIS.AddObject("Line1","Line")
		WITH THIS.Line1 
			.Height = 0
			.Left = 1
			.Top = 3
			.Visible = .t.
			.Width = 1
		ENDWITH
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: MContainerEvent - класс для контейнера формы FileForm (главная часть)
DEFINE CLASS MContainerEvent AS Custom
	&& label2
	PROCEDURE Label2Click()
		oModel_file.Start2("M10870",1)
	ENDPROC 
	
	PROCEDURE Label2Error()
	LPARAMETERS nError, cMethod, nLine
		MESSAGEBOX('mainform@label2error')
	ENDPROC 
	
	&& label3
	PROCEDURE Label3Click()
		oModel_file.Start2("M10860",2)
	ENDPROC
	
	PROCEDURE Label3Error()
	LPARAMETERS nError, cMethod, nLine
		MESSAGEBOX('mainform@label3error')
	ENDPROC 

	&& label4
	PROCEDURE Label4Click()
		oModel_file.Start2("M10880",3)
	ENDPROC
	
	PROCEDURE Label4Error()
	LPARAMETERS nError, cMethod, nLine
		MESSAGEBOX('mainform@label4error')
	ENDPROC
	
	&& label5
	PROCEDURE Label5Click()
		oModel_file.Start2("M10881",4)
	ENDPROC
	
	PROCEDURE Label5Error()
	LPARAMETERS nError, cMethod, nLine
		MESSAGEBOX('mainform@label5error')
	ENDPROC
	
	&& label6
	PROCEDURE Label6Click()
		oModel_file.Start2("M10881K",5)
	ENDPROC
	
	PROCEDURE Label6Error()
	LPARAMETERS nError, cMethod, nLine
		MESSAGEBOX('mainform@label6error')
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: FContainerEvent - класс для контейнера формы FileForm (file input)
DEFINE CLASS FContainerEvent AS Custom
	&& label1
	PROCEDURE Label1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& label2
	PROCEDURE Label2Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC

	&& label3
	PROCEDURE Label3Click()
	LOCAL lcForm
		&& назад
		lcForm = oModel.FindForm("FileForm")
		IF VARTYPE(lcForm) == "O"
			oModel_file.Start("M10870",1)
			lcForm.Container2.Command1.SetFocus() && убрать курсор из text2
		ENDIF
	ENDPROC
	
	PROCEDURE Label3Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& text1
	PROCEDURE Text1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& text2
	PROCEDURE Text2KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lcForm
		&&
		lcForm = oModel.FindForm("FileForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.Container2.Text2.ReadOnly = .t.
		ENDIF
	ENDPROC
	
	PROCEDURE Text2LostFocus()
	LOCAL lcForm
		&&
		lcForm = oModel.FindForm("FileForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.Container2.Text2.ReadOnly = .f.
		ENDIF
	ENDPROC
	
	PROCEDURE Text2Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& command1
	PROCEDURE Command1Click()
	LOCAL lcValue, lcTmp, lcForm, ;
		lnNum, lnForm
		lnForm = oPath.tnForm
		lcForm = oModel.FindForm('FileForm')

		IF VARTYPE(lcForm) == "O"
			lcValue = ALLTRIM(UPPER(lcForm.Container2.Text1.Value))
			IF !EMPTY(lcValue)
				lnNum = RAT(".",lcValue)
				&&
				IF lnNum = 0
					lcValue = ALLTRIM(UPPER(lcValue)) + ALLTRIM(".DBF")
				ENDIF

				IF FILE(lcValue)
					lcForm.Container2.Text2.BackColor = RGB(248,244,255)
					lcForm.Container2.Text2.Value = "Путь к таблице:"
					lcForm.Refresh()
					lcForm.Container2.Text2.ReadOnly = .t.	
					&&
					lcForm.Hide()																															
	
					DO CASE
						CASE lnForm = 1
							&& m10870
							oPath.tcPathFile = ALLTRIM(UPPER(lcForm.Container2.Text1.Value))&&"C:\FOXPRO2\M10870.DBF"
							oModel_m10870.Start()
						
						CASE lnForm = 2
							&& m10860
						
						CASE lnForm = 3
							&& m10880
						
						CASE lnForm = 4
							&& m10881
						
						CASE lnForm = 5
							&& m10881K
					ENDCASE												
				ELSE
					&& файлов нет		
					lcForm.Container2.Text2.BackColor = RGB(255,117,117)				
					lcForm.Container2.Text2.Value = "Нет файла"
					lcForm.Container2.Text2.ReadOnly = .f.
					lcForm.Refresh()
				ENDIF
			ELSE 
				&& файлы не указаны
				lcForm.Container2.Text2.BackColor = RGB(255,117,117)				
				lcForm.Container2.Text2.Value = "Нет файла"
				lcForm.Container2.Text2.ReadOnly = .f.
				lcForm.Refresh()
			ENDIF
		ENDIF						
	ENDPROC 
	
	PROCEDURE Command1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& command2
	PROCEDURE Command2Click()
		QUIT
	ENDPROC
	
	PROCEDURE Command2Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& Class: CustomForm
DEFINE CLASS CustomForm AS Form &&form_one
	BackColor = RGB(219,215,210)
	BorderStyle = 3
	Caption = ""
	Closable = .t.
	Height = SYSMETRIC(22)
	KeyPreview = .t.
	Left = 23
	MaxButton = .t.
	ScrollBars = 0
	ShowInTaskBar = .t.
	ShowTips = .t.
	ShowWindow = 1
	Top = 23
	Visible = .t.
	Width = SYSMETRIC(21)
	WindowState = 2
	WindowType = 0
	
	DIMENSION taIndexColumn(9), ;
			  taFForm(10), ;
		      taPriz(1)	      		     
	&& Копия параметров формы
	tnKeyCnt = 0
	tnId = 0 && Номер операции со строкой в бэкап-таблице
	tnRecno = 0
	tnColIndex = 0
	tnSortNumber = 1
	tnCombo1 = 0
	tnPressSearch = 0
	tnClickSearch = 0
	tcPre = "Перечень изделий" && название формы
	tcAlias = "rtable"
	tcBackupTab = "backupdata_m10870"
	tcBackupAlias = "btable"
	tcTime = .f.&&upd
	tdDate = .f.&&upd
	tlSearch = .f.
			
	PROCEDURE MyBindEvent()
	PARAMETERS tnGridCnt
	LOCAL lcFormT
		FOR EACH myformobject IN THISFORM.Objects
			IF AT(UPPER("grid"),UPPER(myformobject.name)) > 0
				&& выбрали grid
				IF myformobject.TabIndex = tnGridCnt				
					FOR EACH mycolumn IN myformobject.Objects
						FOR EACH mygridobject IN mycolumn.Objects						
							IF ALLTRIM(UPPER(mygridobject.Name)) == UPPER('text1')
								BINDEVENT(mygridobject,"Click",myformobject,"Click")
								BINDEVENT(mygridobject,"KeyPress",myformobject,"KeyPress")
								BINDEVENT(mygridobject,"RightClick",myformobject,"RightClick")
								BINDEVENT(mygridobject,"MouseUp",myformobject,"MouseUp")
							ENDIF
							&&
							IF ALLTRIM(UPPER(mygridobject.Name)) == UPPER('header1')
								IF tnGridCnt = 1
									&&grid1
									FOR EACH mainobject IN Application.Objects
										IF ALLTRIM(UPPER(mainobject.Name)) == UPPER('columnheader')
											BINDEVENT(mygridobject,"Click",oColumnHeader,"Click")
										ENDIF
									ENDFOR
								ENDIF
							ENDIF
						ENDFOR
					ENDFOR
				ENDIF
			ENDIF
		ENDFOR
	ENDPROC
	
	PROCEDURE MyAfterRowColChange()&&upd
		SELECT ALIAS()
		GOTO (THISFORM.tnRecno) IN ALIAS()
		SCATTER MEMVAR		
		REPLACE rtable.izm WITH m.izm, ;
				rtable.nizd WITH m.nizd, ;
				rtable.snizd WITH m.snizd, ;
				rtable.mod WITH m.mod, ;
				rtable.kudar WITH m.kudar, ;
				rtable.kuda WITH m.kuda, ;
				rtable.dtv WITH m.dtv, ;
				rtable.cex WITH m.cex, ;
				rtable.rank WITH m.rank, ;
				rtable.kd WITH m.kd, ;
				rtable.priz WITH m.priz, ;
				rtable.naim WITH m.naim
	ENDPROC
	
	PROCEDURE GetIdn
	LOCAL lnIdn
		lnIdn = THIS.tnId
		RETURN lnIdn	
	ENDPROC
	
	PROCEDURE SetIdn
	PARAMETERS tnIdn	
		THIS.tnId = tnIdn
	ENDPROC
	
	PROCEDURE Init()
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.HighlightStyle = 2
				myobject.HighlightBackColor = oPath.tnGridColor
				myobject.HighlightForeColor = oPath.tnGridTextColor
			ENDIF
		ENDFOR
		&&
		THISFORM.Height = oPath.tnScreenHeight&&upd
		THISFORM.Refresh()						
	ENDPROC
	
	PROCEDURE Resize()
	LOCAL lcForm, lcAliasname, ;
	lnPanelLeft, lnPanelTop
		IF THISFORM.WindowState = 0
			&& n
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
					myobject.Width = THISFORM.Width
					myobject.Height = THISFORM.Height - 34
				ENDIF
			ENDFOR	
			IF THISFORM.Height != oPath.tnScreenHeight AND THISFORM.WindowState = 0&&upd
				oPath.SetMyParam("tnScreenHeight",THISFORM.Height)
			ENDIF		
		ENDIF
		
		IF THISFORM.WindowState = 2
			&& max
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
					myobject.Width = THISFORM.Width + 2
					myobject.Height = THISFORM.Height - 33
				ENDIF
			ENDFOR						
		ENDIF
		
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.Top = 34
			ENDIF
		ENDFOR
		&&
		THISFORM.Container1.Left = 0
		THISFORM.Container1.Top = 0
		THISFORM.Container1.Width = THISFORM.Width
		THISFORM.Container1.Height = 34
		THISFORM.Container1.Resize()
		&& m10870
		oPath.SetCustomFont()
	ENDPROC
	
	PROCEDURE Valid()&&upd
		THISFORM.MyAfterRowColChange()
	ENDPROC
	
	PROCEDURE Unload()&&
		*oModel_m10870.FixKudarAll()&&?
		oModel_m10870.DateTxt()&&upd
		oModel_m10870.DeleteBackup()&&upd
		oModel_m10870.RepeatingRecords()
	ENDPROC

	PROCEDURE QueryUnload()
		QUIT
	ENDPROC
	
	PROCEDURE Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC

	&& Grid1
	ADD OBJECT Grid1 AS Grid WITH Name = "Grid1", ;
	AllowCellSelection = .t., BackColor = RGB(248,244,255), ColumnCount = -1, ;
	DeleteMark = .f., Height = 428, HighlightStyle = 2, Left = 0, ReadOnly = .f., ; 
	RecordSourceType = 1, RowHeight = 21, ScrollBars = 3, Top = 48, View = 0, Visible = .t., Width = 640
 	
 	PROCEDURE Grid1.MyObject()
 		RETURN THIS
 	ENDPROC
 	
 	PROCEDURE Grid1.MyKeyPress()
 	PARAMETERS tnKeyCode
 	LOCAL lcSymbol, lcTemp, ;
	lnColIndex, lnAnswer, lnSelStart, lnSelLength
		lnColIndex = THISFORM.tnColIndex
		lnAnswer = tnKeyCode&&LASTKEY()
		SELECT ALIAS()
		lnSelStart = THIS.Columns(lnColIndex).Text1.SelStart
		lnSelLength = THIS.Columns(lnColIndex).Text1.SelLength
		LOCAL lnMySelLength
		&&
		DO CASE
			CASE lnAnswer = 1
				&& ctrl+a
				oCell.GridSelectAll()
				
			CASE lnAnswer = 3
				&& ctrl+c
				oCell.GridCopyOption()
			
			CASE lnAnswer = 22
				&& ctrl+v
				oCell.GridPasteOption()
			
			CASE lnAnswer = 24
				&& ctrl+x
				oCell.GridCutOption()
		ENDCASE
		
		lcTemp = oCell.GetValue()
		IF lnColIndex = 5 AND lnAnswer = 22 OR lnColIndex = 5 AND lnAnswer = 24
			&& обновление грид
			IF VARTYPE(lcTemp) = "C"				
				REPLACE kuda WITH icod(lcTemp) IN "rtable"										
				THIS.Columns(lnColIndex + 1).Text1.Value = icod(lcTemp)
			ENDIF	
		ENDIF
 	ENDPROC
 	
 	PROCEDURE Grid1.GetMouseObject()
 	LPARAMETERS tcClass	
	LOCAL loRet
	&& Получение ссылки на объект под курсором
		IF EMPTY(tcClass)
			tcClass = "Column"
		ENDIF

		LOCAL ARRAY laM[2]
		AMOUSEOBJ(laM, 1)
		IF TYPE("laM[1]") = "O" 
			IF laM[1].BaseClass = tcClass
				loRet = laM[1]
			ELSE
				IF TYPE("laM[1].ControlCount") = "N"
					FOR i = 1 TO laM[1].ControlCount
						IF laM[1].Controls(i).BaseClass = tcClass
							loRet = laM[1].Controls(i)
							EXIT 
						ENDIF 
					ENDFOR 
				ENDIF 
			ENDIF 
		ENDIF 
		&&
		RETURN loRet
 	ENDPROC
 
 	PROCEDURE Grid1.Init()
 		THIS.Refresh()
 	ENDPROC
 	
 	PROCEDURE Grid1.Click()
		&& Сохраняем номер выбранной строки для поиска по связанным таблицам
		THISFORM.tlSearch = .f.
		&&		
		SELECT "rtable"
		THISFORM.tnRecno = RECNO("rtable")&&RECNO(ALIAS())
		GOTO (THISFORM.tnRecno) IN "rtable"
		*oModel_m10870.FixKudar()&&
 	ENDPROC
 	
 	PROCEDURE Grid1.KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lcSymbol, lcTemp, ;
	lnColIndex, lnAnswer, lnSelStart, lnSelLength
	lnColIndex = THISFORM.tnColIndex
		lnAnswer = LASTKEY()
		SELECT ALIAS()
		THISFORM.tnRecno = RECNO(ALIAS())
		lnSelStart = THIS.Columns(lnColIndex).Text1.SelStart
		lnSelLength = THIS.Columns(lnColIndex).Text1.SelLength
		&&
		IF nShiftAltCtrl != 2&&upd
			&& other
			lcSymbol = CHR(nKeyCode)
		ENDIF
		&&
		IF nShiftAltCtrl != 2&&upd ctrl+c не изменение, а ctrl-v?
			IF EMPTY(rtable.izm)
				REPLACE rtable.izm WITH "1"
				THIS.Column1.Text1.Value = "1"
			ENDIF
		ENDIF
	
		IF lnAnswer = 2 OR lnAnswer = 22 OR lnAnswer >= 32 AND lnAnswer < 127 OR ;
			lnAnswer >= 192 AND lnAnswer <= 255 OR nShiftAltCtrl = 1 AND lnAnswer >= 192 AND lnAnswer <= 223
				IF nShiftAltCtrl != 1 AND lnAnswer != 50 OR nShiftAltCtrl != 1 AND lnAnswer != 52 OR nShiftAltCtrl != 1 AND lnAnswer != 54 OR ;
				nShiftAltCtrl != 1 AND lnAnswer != 56 OR nShiftAltCtrl = 1 AND lnAnswer >= 192 AND lnAnswer <= 223
					&& other
					IF THIS.Columns(lnColIndex).Text1.SelLength > 0
						&& есть выделенная часть, замена того, что есть, на то, что вводим
						lcTemp = SUBSTR(THIS.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
						ALLTRIM(lcSymbol) + ;
						SUBSTR(THIS.Columns(lnColIndex).Text1.Value, lnSelStart + lnSelLength + 1)
					ELSE
						&& обычное нажатие и вставка
						lcTemp = SUBSTR(THIS.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
						ALLTRIM(lcSymbol) + ;
						SUBSTR(THIS.Columns(lnColIndex).Text1.Value, lnSelStart + 1)										
					ENDIF
				ENDIF
		ELSE
			IF nShiftAltCtrl != 1 AND lnAnswer != 50 AND lnAnswer != 52 AND lnAnswer != 54 AND ;
				lnAnswer != 56	
				IF lnAnswer == 7
					&& delete
					IF THIS.Columns(lnColIndex).Text1.SelLength > 0
						&& есть выделенный текст, удаляем только его
						lcTemp = SUBSTR(THIS.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
						SUBSTR(THIS.Columns(lnColIndex).Text1.Value, lnSelStart + 1 + lnSelLength)						
					ELSE
						&& обычное нажатие delete
						lcTemp = SUBSTR(THIS.Columns(lnColIndex).Text1.Value, 1, lnSelStart) + ;
						SUBSTR(THIS.Columns(lnColIndex).Text1.Value, lnSelStart + 2)											
					ENDIF
				ENDIF

				IF lnAnswer == 127
					&& backspace
					IF THIS.Columns(lnColIndex).Text1.SelLength > 0
						&& есть выделенный текст, удаляем только его
						lcTemp = SUBSTR(THIS.Columns(lnColIndex).Text1.Value, 1, lnSelStart - 1) + ;
						SUBSTR(THIS.Columns(lnColIndex).Text1.Value, lnSelStart + lnSelLength)
						lnSelStart = lnSelStart + 1 &&
					ELSE
						&& обычное нажатие backspace
						lcTemp = SUBSTR(THIS.Columns(lnColIndex).Text1.Value, 1, lnSelStart - 1) + ;
						SUBSTR(THIS.Columns(lnColIndex).Text1.Value, lnSelStart + 1)			
					ENDIF
				ENDIF
			ENDIF															
		ENDIF
		
		IF VARTYPE(lcTemp) = "C"&&upd
			oModel_m10870.BackupData("keypress",lcTemp)
		ENDIF
		&&
		IF lnColIndex = 5
			&& обновление грид
			IF VARTYPE(lcTemp) = "C"				
				REPLACE kuda WITH icod(lcTemp) IN "rtable"
				THIS.Columns(lnColIndex + 1).Text1.Value = icod(lcTemp)
			ENDIF						
		ENDIF
				
		DO CASE					
			CASE lnAnswer = 1 
				&& HOME
				IF !THISFORM.tlSearch
					GO TOP IN ALIAS()
					THIS.SetFocus()
				ENDIF

			CASE lnAnswer = 6
				&& END
				IF !THISFORM.tlSearch
					GO BOTTOM IN ALIAS()
					THIS.SetFocus()
				ENDIF
		ENDCASE	
	ENDPROC
 
 	PROCEDURE Grid1.AfterRowColChange()
 	LPARAMETERS nColIndex
		&& Номер элемента массива, который хранит дескриптор формы
		*THISFORM.tnRecno = RECNO(ALIAS())&&upd
		THISFORM.tnColIndex = nColIndex
		oModel_m10870.BackupData("afterrowcolchange","")
 	ENDPROC
 	
 	PROCEDURE Grid1.Valid()&&upd
 		THISFORM.MyAfterRowColChange()
 	ENDPROC
 	 	
 	PROCEDURE Grid1.MouseUp()
 	LPARAMETERS nButton, nShift, nXCoord, nYCoord
 		IF nButton = 2
			oModel.DoContext()
		ENDIF
 	ENDPROC
 	
 	PROCEDURE Grid1.Error()
 	LPARAMETERS nError, cMethod, nLine
		SELECT 1
 	ENDPROC
 	
 	&& Container1
 	ADD OBJECT Container1 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .t.

	PROCEDURE Container1.AddMyItem()
		&& delete all
		FOR i = 1 TO THIS.Combo1.ListCount
			THIS.Combo1.RemoveListItem(i)
		ENDFOR
				
		&& add
		THIS.Combo1.AddItem("№ изделия") && 1
		THIS.Combo1.AddItem("Цех+№ изделия") && 2
		THIS.Combo1.AddItem("Код+№ изделия") && 3
		THIS.Combo1.AddItem("№ изд.(ст.)") && 4
		THIS.Combo1.AddItem("ДСЕ (внешний)") && 5
		THIS.Combo1.AddItem("ДСЕ (внутренний)") && 6
		THIS.Combo1.AddItem("Наименование")&& 7
		THIS.Combo1.AddItem("Признак") && 8
		THIS.Combo1.AddItem("Без индекса") && 9
		&&
		DIMENSION THISFORM.taIndexColumn(9)
		THISFORM.taIndexColumn[1] = 2
		THISFORM.taIndexColumn[2] = 3
		THISFORM.taIndexColumn[3] = 10
		THISFORM.taIndexColumn[4] = 6
		THISFORM.taIndexColumn[5] = 7
		THISFORM.taIndexColumn[6] = 1
		THISFORM.taIndexColumn[7] = 8
		THISFORM.taIndexColumn[8] = 11
		THISFORM.taIndexColumn[9] = 0
		
		THIS.Combo1.Value = 1
		THISFORM.tnCombo1 = THIS.Combo1.Value
		THIS.Combo1.DisplayCount = THIS.Combo1.ListCount
	ENDPROC

	PROCEDURE Container1.Init()
		&& Combo1
		THIS.AddObject("Combo1","ComboBox")
		WITH THIS.Combo1
			.Enabled = .t.
			.Height = 26
			.ReadOnly = .f.
			.Style = 2
			.ToolTipText = "Индекс сортировки"
			.Visible = .t.
			.Width = 175
		ENDWITH
		&&
		BINDEVENT(THIS.Combo1,"Click",oCContainerEvent,"Combo1Click")
		BINDEVENT(THIS.Combo1,"Error",oCContainerEvent,"Combo1Error")
		BINDEVENT(THIS.Combo1,"MouseMove",oCContainerEvent,"Combo1MouseMove")
			
		&& Text1
		THIS.AddObject("Text1","TextBox")
		WITH THIS.Text1 
			.Alignment = 0
			.BackColor = RGB(248,244,255)
			.Enabled = .t.
			.FontName = 'Consolas'
			.FontSize = 11
			.Height = 26
			.ToolTipText = "Укажите, что искать (Ctrl+F)"
			.Value = "Поиск"
			.Visible = .t.
			.Width = 202
			&&
			PUBLIC gbTextMenu1
			gbTextMenu1 = .f.
		ENDWITH
		&&
		BINDEVENT(THIS.Text1,"Click",oCContainerEvent,"Text1Click")
		BINDEVENT(THIS.Text1,"KeyPress",oCContainerEvent,"Text1KeyPress")
		BINDEVENT(THIS.Text1,"Error",oCContainerEvent,"Text1Error")
		*BINDEVENT(THIS.Text1,"RightClick",oCContainerEvent,"Text1RightClick")
		BINDEVENT(THIS.Text1,"LostFocus",oCContainerEvent,"Text1LostFocus")
		BINDEVENT(THIS.Text1,"MouseUp",oCContainerEvent,"Text1MouseUp")
		
		&& Command1
		THIS.AddObject("Command1","CommandButton")
		WITH THIS.Command1
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\search.png"
			.PictureMargin = 0
			.PicturePosition = 14
			.ToolTipText = "Поиск по таблице (Enter)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command1,"Click",oCContainerEvent,"Command1Click")
		BINDEVENT(THIS.Command1,"Error",oCContainerEvent,"Command1Error")
		
		&& Command2
		THIS.AddObject("Command2","CommandButton")
		WITH THIS.Command2
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\back.png"
			.PictureMargin = 0
			.PicturePosition = 13
			.ToolTipText = "Вернуть прежнее значение (Ctrl+Z)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command2,"Click",oCContainerEvent,"Command2Click")
		BINDEVENT(THIS.Command2,"Error",oCContainerEvent,"Command2Error")
		
		&& Command3
		THIS.AddObject("Command3","CommandButton")
		WITH THIS.Command3
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\add.png"
			.PictureMargin = 0
			.PicturePosition = 13
			.ToolTipText = "Добавить новую строку (Ctrl+N)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command3,"Click",oCContainerEvent,"Command3Click")
		BINDEVENT(THIS.Command3,"Error",oCContainerEvent,"Command3Error")
				
		&& Line
		LOCAL lnCnt,loTemp
		lnCnt = 1
		FOR i = 1 TO 4&&5
			THIS.AddObject("Line"+ALLTRIM(STR(lnCnt)),"Line")
			loTemp = GETPEM(THIS,"Line"+ALLTRIM(STR(lnCnt)))
			WITH loTemp
				.Height = 26
				.Width = 0
				.Visible = .t.
			ENDWITH			
			lnCnt = lnCnt + 1
		ENDFOR
		&&
		THIS.AddMyItem()
	ENDPROC
	
	PROCEDURE Container1.Resize()
	LOCAL lnPanelLeft,lnPanelTop
		lnPanelTop = 4
		&&
		THIS.Combo1.Left = 19
		lnPanelLeft = THIS.Combo1.Left + THIS.Combo1.Width
		THIS.Combo1.Top = 4
		&&
		THIS.Line1.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Line1.Left + THIS.Line1.Width
		THIS.Line1.Top = lnPanelTop
		&&
		THIS.Text1.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Text1.Left + THIS.Text1.Width
		THIS.Text1.Top = lnPanelTop
		&&
		THIS.Line2.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Line2.Left + THIS.Line2.Width
		THIS.Line2.Top = lnPanelTop
		&&
		THIS.Command1.Left = lnPanelLeft + 5 && Поиск
		lnPanelLeft = THIS.Command1.Left + THIS.Command1.Width
		THIS.Command1.Top = lnPanelTop
		&&
		THIS.Line3.Left = lnPanelLeft + 4 
		lnPanelLeft = THIS.Line3.Left + THIS.Line3.Width
		THIS.Line3.Top = lnPanelTop
		&&
		THIS.Command2.Left = lnPanelLeft + 5 && Отменить изменения
		lnPanelLeft = THIS.Command2.Left + THIS.Command2.Width
		THIS.Command2.Top = lnPanelTop
		&&
		THIS.Line4.Left = lnPanelLeft + 4
		lnPanelLeft = THIS.Line4.Left + THIS.Line4.Width
		THIS.Line4.Top = lnPanelTop
		&&
		THIS.Command3.Left = lnPanelLeft + 5 && Добавить строку
		lnPanelLeft = THIS.Command3.Left + THIS.Command3.Width
		THIS.Command3.Top = lnPanelTop
	ENDPROC	
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: CContainerEvent - класс для контейнера формы CustomForm
DEFINE CLASS CContainerEvent AS Custom
	PROCEDURE Combo1Click()
	LOCAL lcForm, lcTxt
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			IF lcForm.Grid1.Visible
				SELECT "rtable"
				SCATTER MEMVAR
				&&
				lcTxt = TRIM(m.izm) + " " + ;				
					TRIM(m.nizd) + " " + ;
					TRIM(m.snizd) + " " + ;
					TRIM(m.mod) + " " + ;
					TRIM(m.kudar) + " " + ;
					TRIM(m.kuda) + " " + ;
					TRIM(DTOC(m.dtv)) + " " + ;
					TRIM(m.cex) + " " + ;
					TRIM(m.rank) + " " + ;
					TRIM(m.kd) + " " + ;
					TRIM(m.priz) + " " + ;
					TRIM(m.naim)
				&&
				oModel_m10870.SearchRecord(lcTxt, lcForm.Container1.Combo1.ListIndex) &&THIS
				lnRecno = lcForm.tnRecno
				GOTO (lnRecno) IN "rtable"
				lnColIndex = lcForm.tnColIndex
				lcForm.tnCombo1 = lcForm.Container1.Combo1.ListIndex
				lcForm.Grid1.Columns(lnColIndex).Text1.SetFocus()
				lcForm.Grid1.SetFocus()
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE Combo1MouseMove()
	LPARAMETERS nButton, nShift, nXCoord, nYCoord
		NODEFAULT
	ENDPROC
	
	PROCEDURE Combo1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	PROCEDURE Text1Click()
	LOCAL lcForm		
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.
			IF lcForm.tnClickSearch = 0
				lcForm.Container1.Text1.Value = ""
				lcForm.tnPressSearch = lcForm.tnPressSearch + 1
				lcForm.tnClickSearch = lcForm.tnClickSearch + 1
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE Text1KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lcForm	
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.
			IF lcForm.tnPressSearch = 0	
				lcForm.Container1.Text1.Value = ""
				lcForm.tnPressSearch = lcForm.tnPressSearch + 1
				lcForm.tnClickSearch = lcForm.tnClickSearch + 1
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE Text1MyKeyPress()
	PARAMETERS tnKeyCode
	LOCAL lcForm,lcTemp,lcSymbol, ;
	lnSelStart,lnSelLength, ;
	loNode
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			loNode = oModel.MyObject()
			lnSelStart = loNode.SelStart
			lnSelLength = loNode.SelLength
			&&
			DO CASE 
				CASE tnKeyCode = 1
					&& ctrl+a
					oCell.TextSelectAll()
					
				CASE tnKeyCode = 3
					&& ctrl+c
					gbTextMenu1 = .t.
					oCell.TextCopyOption()
					lcForm.Container1.Text1.SetFocus()
				
				CASE tnKeyCode = 22
					&& ctrl+v
					gbTextMenu1 = .t.
					oCell.TextPasteOption()
				
				CASE tnKeyCode = 24
					&& ctrl+x
					oCell.TextCutOption()
			ENDCASE			
		ENDIF
	ENDPROC
	
	PROCEDURE Text1MouseUp()
 	LPARAMETERS nButton, nShift, nXCoord, nYCoord
 	LOCAL lcForm
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.			
			IF nButton = 2
	 			IF lcForm.tnClickSearch = 0
					lcForm.Container1.Text1.Value = ""
					lcForm.tnPressSearch = lcForm.tnPressSearch + 1
					lcForm.tnClickSearch = lcForm.tnClickSearch + 1
				ENDIF
				oModel.DoText()
			ENDIF
		ENDIF	
 	ENDPROC
	
	PROCEDURE Text1LostFocus()
	LOCAL lcForm
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"	
			IF TYPE("gbTextMenu1") != "U"
				IF gbTextMenu1
					&& чтобы фокус не уходил из text1
					gbTextMenu1 = .f.
					NODEFAULT			
					lcForm.Container1.Text1.SetFocus()
				ENDIF
			ENDIF
		ENDIF	
	ENDPROC
	
	PROCEDURE Text1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& command1 - поиск по таблице
	PROCEDURE Command1Click() 
	LOCAL lcForm, lcValue, lcChoiceSearch
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"		
			lcValue = ALLTRIM(UPPER(lcForm.Container1.Text1.Value))
			IF lcForm.Grid1.Visible
				IF !EMPTY(lcValue)		
					DO CASE 			
						CASE lcForm.Container1.Combo1.ListIndex = 1		
							&& nizd
							oModel_m10870.Search(2,lcValue)

						CASE lcForm.Container1.Combo1.ListIndex = 2
							&& cn
							oModel_m10870.Search(3,lcValue)
						
						CASE lcForm.Container1.Combo1.ListIndex = 3
							&& kdn
							oModel_m10870.Search(10,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 4
							&& snizd
							oModel_m10870.Search(6,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 5
							&& kdr
							oModel_m10870.Search(7,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 6
							&& kd
							oModel_m10870.Search(1,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 7
							&& naim
							oModel_m10870.Search(8,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 8
							&& priz
							oModel_m10870.Search(11,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 9
							&& no
							oModel_m10870.Search(0,lcValue)
					ENDCASE
				ENDIF
				&&
				lcForm.Grid1.Refresh()
			ENDIF			
		ENDIF
	ENDPROC
			
	PROCEDURE Command1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	PROCEDURE Command2Click()
		&& вернуть прежнее значение
		oModel_m10870.RollbackData()
	ENDPROC
	
	PROCEDURE Command2Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	PROCEDURE Command3Click()
	LOCAL lcForm, lcFormT	
		&& Добавить новую строку
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.Container1.Combo1.ListIndex = lcForm.Container1.Combo1.ListCount
			SELECT "rtable"
			SCATTER MEMVAR 
			SET ORDER TO 
			APPEND BLANK 
			GO BOTTOM IN ALIAS()		
									
			lnWorkArea = oModel.IsUsed("rqcursor")
			IF lnWorkArea > 0 &&upd				
				FOR EACH myobject IN lcForm.Controls
					IF UPPER(ALLTRIM(myobject.BaseClass)) == UPPER("grid")
						SELECT (lnWorkArea)
						SCAN
							IF UPPER(ALLTRIM(rqcursor.value)) == UPPER("да")
								DO CASE 
									CASE RECNO(lnWorkArea) = 1
										myobject.Column1.Text1.Value = "1"
										REPLACE rtable.izm WITH "1" && m.izm
										
									CASE RECNO(lnWorkArea) = 2										
										myobject.Column2.Text1.Value = m.nizd
										REPLACE rtable.nizd WITH m.nizd
									
									CASE RECNO(lnWorkArea) = 3										
										myobject.Column3.Text1.Value = m.snizd
										REPLACE rtable.snizd WITH m.snizd
									
									CASE RECNO(lnWorkArea) = 4										
										myobject.Column4.Text1.Value = m.mod
										REPLACE rtable.mod WITH m.mod
									
									CASE RECNO(lnWorkArea) = 5										
										myobject.Column5.Text1.Value = m.kudar
										REPLACE rtable.kudar WITH m.kudar
									
									CASE RECNO(lnWorkArea) = 6										
										myobject.Column6.Text1.Value = m.kuda
										REPLACE rtable.kuda WITH m.kuda
									
									CASE RECNO(lnWorkArea) = 7										
										myobject.Column7.Text1.Value = DATE()
										REPLACE rtable.dtv WITH DATE() && m.dtv
									
									CASE RECNO(lnWorkArea) = 8										
										myobject.Column8.Text1.Value = m.cex
										REPLACE rtable.cex WITH m.cex
									
									CASE RECNO(lnWorkArea) = 9										
										myobject.Column9.Text1.Value = m.rank
										REPLACE rtable.rank WITH m.rank
									
									CASE RECNO(lnWorkArea) = 10										
										myobject.Column10.Text1.Value = m.kd
										REPLACE rtable.kd WITH m.kd
									
									CASE RECNO(lnWorkArea) = 11										
										myobject.Column11.Text1.Value = m.priz
										REPLACE rtable.priz WITH m.priz
									
									CASE RECNO(lnWorkArea) = 12										
										myobject.Column12.Text1.Value = m.naim
										REPLACE rtable.naim WITH m.naim
								ENDCASE
							ENDIF
						ENDSCAN
						&&
						GO BOTTOM IN "rtable"
						myobject.SetFocus()										
						EXIT
					ENDIF
				ENDFOR				
			ENDIF						
		ENDIF
	ENDPROC
	
	PROCEDURE Command3Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: ColumnHeader - класс header для грида (для обработки событий)
DEFINE CLASS ColumnHeader AS Custom
	PROCEDURE Click()
	LOCAL lcForm, ;
	loHeader,loColumn
		lcForm = oModel.FindForm("CustomForm")
		loHeader = lcForm.Grid1.GetMouseObject('Header')
		DIMENSION laThisObject(1)
		AMEMBERS(laThisObject,loHeader,1)
		loColumn = loHeader.Parent
		&&
		DO CASE
			CASE loColumn.ColumnOrder = 2
				&& № изделия
				lcForm.Container1.Combo1.ListIndex = 1
				
			CASE loColumn.ColumnOrder = 8
				&& Цех+№ изделия
				lcForm.Container1.Combo1.ListIndex = 2
				
			CASE loColumn.ColumnOrder = 10
				&& Код+№ изделия
				lcForm.Container1.Combo1.ListIndex = 3
				
			CASE loColumn.ColumnOrder = 3
				&& № изд.(ст.)
				lcForm.Container1.Combo1.ListIndex = 4
				
			CASE loColumn.ColumnOrder = 5
				&& ДСЕ (внешний)
				lcForm.Container1.Combo1.ListIndex = 5
				
			CASE loColumn.ColumnOrder = 6
				&& ДСЕ (внутренний)
				lcForm.Container1.Combo1.ListIndex = 6
				
			CASE loColumn.ColumnOrder = 12
				&& Наименование
				lcForm.Container1.Combo1.ListIndex = 7
				
			CASE loColumn.ColumnOrder = 11
				&& Признак
				lcForm.Container1.Combo1.ListIndex = 8
		ENDCASE
		&&
		IF loColumn.ColumnOrder > 1 AND loColumn.ColumnOrder < 4 OR ;
		loColumn.ColumnOrder > 4 AND loColumn.ColumnOrder < 7 OR ;
		loColumn.ColumnOrder = 8 OR loColumn.ColumnOrder > 9 AND loColumn.ColumnOrder < 13
			lcForm = oModel.FindClass("CContainerEvent")&& 2 3 5 6 8 10 11 12
			IF VARTYPE(lcForm) == "O"
				lcForm.Combo1Click()
			ENDIF
		ENDIF
	ENDPROC
	
*!*		PROCEDURE ClickSaveCustom()
*!*		LOCAL lcForm, ;
*!*		loHeader,loColumn
*!*			lcForm = oModel.FindForm("SaveCustomForm") 
*!*			loHeader = lcForm.Grid1.GetMouseObject('Header')
*!*			DIMENSION laThisObject(1)
*!*			AMEMBERS(laThisObject,loHeader,1)
*!*			loColumn = loHeader.Parent
*!*			
*!*			DO CASE
*!*				CASE loColumn.ColumnOrder = 2
*!*					&& nizd
*!*					lcForm.Container1.Combo1.ListIndex = 1
*!*					
*!*				CASE loColumn.ColumnOrder = 3
*!*					&& kod
*!*					lcForm.Container1.Combo1.ListIndex = 2
*!*					
*!*				CASE loColumn.ColumnOrder = 8
*!*					&& naim
*!*					lcForm.Container1.Combo1.ListIndex = 3
*!*			ENDCASE
*!*			
*!*			IF loColumn.ColumnOrder = 2 OR loColumn.ColumnOrder = 3 OR ;
*!*			loColumn.ColumnOrder = 8
*!*				lcForm = oModel.FindClass("SCContainerEvent")
*!*				IF VARTYPE(lcForm) == "O"
*!*					lcForm.Combo1Click()
*!*				ENDIF
*!*			ENDIF
*!*		ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: RequisitesForm - класс формы RequisitesForm
DEFINE CLASS RequisitesForm AS Form
	AlwaysOnTop = .t.
	BackColor = RGB(219,215,210)
	BorderStyle = 3
	Left = 1
	MaxButton = .t.&&f
	ShowTips = .t.
	Top = 1	
	Visible = .f.
	ShowWindow = 0

	DIMENSION taRequisites(1), ;
			  taRequisitesAnswer(1)
	tnColIndex = 1
	tcPre = "Реквизиты"
	taRequisites = ""
	
	PROCEDURE MyBindEvent()
	PARAMETERS tnGridCnt
	LOCAL lcFormT
		FOR EACH myformobject IN THISFORM.Objects
			IF AT(UPPER("grid"),UPPER(myformobject.name)) > 0
				&& выбрали grid
				IF myformobject.TabIndex = tnGridCnt				
					FOR EACH mycolumn IN myformobject.Objects
						FOR EACH mygridobject IN mycolumn.Objects						
							IF ALLTRIM(UPPER(mygridobject.Name)) == UPPER('text1')
								BINDEVENT(mygridobject,"Click",myformobject,"Click")
							ENDIF
						ENDFOR
					ENDFOR
				ENDIF
			ENDIF
		ENDFOR
	ENDPROC
	
	PROCEDURE MyAfterRowColChange()&&upd
	ENDPROC
	
	PROCEDURE Init()
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.HighlightStyle = 2
				myobject.HighlightBackColor = oPath.tnGridColor
				myobject.HighlightForeColor = oPath.tnGridTextColor
			ENDIF
		ENDFOR
		&&
		LOCAL lcForm,lcTabname,lcAliasname, ;
		lnWorkArea
		lcForm = oModel.FindForm("FileForm")
		IF VARTYPE(lcForm) == "O"
			THISFORM.Width = lcForm.Width &&upd
		ENDIF
		
		lcForm = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			lcTabname = oPath.tcPathFile
			lcAliasname = lcForm.tcAlias
			lnWorkArea = oModel.CompareWorkArea(lcTabname,lcAliasname)
			
			IF lnWorkArea > 0
				DIMENSION laArray(1)	
				DIMENSION THISFORM.taRequisites(AFIELDS(laArray,lnWorkArea))
			ENDIF			
		ENDIF								
		&&
		THISFORM.Width = oPath.tnReqFormWidth&&upd	
		THISFORM.Height = oPath.tnReqFormHeight
		THISFORM.Refresh()
	ENDPROC
	
	PROCEDURE Resize()
	LOCAL lcForm, lcAliasname, ;
	lnPanelLeft, lnPanelTop
		IF THISFORM.WindowState = 0
			&& n
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
					IF THISFORM.Width != oPath.tnReqFormWidth&&upd
						oPath.SetMyParam("tnReqFormWidth",THISFORM.Width)
					ENDIF
					
					IF THISFORM.Height != oPath.tnReqFormHeight&&upd
						oPath.SetMyParam("tnReqFormHeight",THISFORM.Height)
					ENDIF
					
					myobject.Width = THISFORM.Width 
					myobject.Height = THISFORM.Height&&myobject.HeaderHeight + myobject.RowHeight * (RECCOUNT("rqcursor") + 1)				
				ENDIF
			ENDFOR
		ENDIF
		
		IF THISFORM.WindowState = 2
			&& max
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
					myobject.Width = THISFORM.Width + 2
					IF THISFORM.Height > 0
					myobject.Height = THISFORM.Height - 33
					ENDIF
				ENDIF
			ENDFOR
		ENDIF
		
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.Top = 34
			ENDIF
		ENDFOR
		&&
		THISFORM.Container1.Left = 0
		THISFORM.Container1.Top = 0
		THISFORM.Container1.Width = THISFORM.Width
		THISFORM.Container1.Height = 34
		THISFORM.Container1.Resize()
		&& m10870
		oPath.SetCustomFont()
	ENDPROC
	
	PROCEDURE QueryUnload()
		*oPath.DoMenu(1,2,3)&&upd
		oPath.MarkMenu()
		FormDelete("requisitesform")&& в queryunload не должно быть зависимостей от переменных - они удаляются раньше, чем происходит повторный запрос
		THISFORM.Hide()
		THISFORM.Destroy()
	ENDPROC
	
	PROCEDURE Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& Grid1
	ADD OBJECT Grid1 AS Grid WITH Name = "Grid1", ;
	AllowCellSelection = .t., BackColor = RGB(248,244,255), ColumnCount = -1, ;
	DeleteMark = .f., Height = 100, HighlightStyle = 2, Left = 0, ReadOnly = .f., ;
	RecordSourceType = 1, RowHeight = 21, ScrollBars = 3, Top = 48, Visible = .t., Width = 100
	
	PROCEDURE Grid1.AfterRowColChange()
 	LPARAMETERS nColIndex
		&& Номер элемента массива, который хранит дескриптор формы
		THISFORM.tnColIndex = nColIndex
 	ENDPROC
	
	PROCEDURE Grid1.Click()
	LOCAL lnColIndex,lnWorkArea	
		lnColIndex = THISFORM.tnColIndex
		&&
		IF lnColIndex = 2
			lnWorkArea = oModel.IsUsed("rqcursor")
			IF lnWorkArea > 0
				LOCAL lcForm
				FOR EACH myobject IN THISFORM.Controls
					IF UPPER(ALLTRIM(myobject.BaseClass)) == UPPER("grid")
						lcForm = myobject
						EXIT
					ENDIF
				ENDFOR

				IF VARTYPE(lcForm) == "O"
					SELECT (lnWorkArea)
					LOCAL lcAnswer
					IF UPPER(ALLTRIM(value)) = UPPER("да")
						&& да->нет
						lcAnswer = "Нет"
						
					ELSE 
						IF UPPER(ALLTRIM(value)) = UPPER("нет")
							&& нет->да
							lcAnswer = "Да"
						ENDIF					
					ENDIF 
					&&
					REPLACE value WITH (lcAnswer)
					lcForm.Columns(lnColIndex).Text1.Value = (lcAnswer)
					&& изменение среди реквизитов, которые отображаем
					LOCAL lcAnswer,lnRecno
					lcAnswer = ""
					lnRecno = RECNO(lnWorkArea)
					SCAN
						lcAnswer = lcAnswer + value + ","
					ENDSCAN
					&&
					oPath.tcRequisitesAnswer = lcAnswer
					oPath.SetMyParam("tcRequisitesAnswer ",oPath.tcRequisitesAnswer)
					GOTO (lnRecno) IN (lnWorkArea)
				ENDIF
			ENDIF			
		ENDIF
	ENDPROC
	
	&& Container1
 	ADD OBJECT Container1 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .t.
 	
 	PROCEDURE Container1.Init() 			
		&& Command1
		THIS.AddObject("Command1","CommandButton")
		WITH THIS.Command1
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\asterisk.png"
			.PictureMargin = 0
			.PicturePosition = 14
			.ToolTipText = "Выбрать все/Отменить выбор (F11/F12)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command1,"Click",oRContainerEvent,"Command1Click")
		BINDEVENT(THIS.Command1,"Error",oRContainerEvent,"Command1Error")
		
		&& Line
		LOCAL lnCnt,loTemp
		lnCnt = 1
		FOR i = 1 TO 1
			THIS.AddObject("Line"+ALLTRIM(STR(lnCnt)),"Line")
			loTemp = GETPEM(THIS,"Line"+ALLTRIM(STR(lnCnt)))
			WITH loTemp
				.Height = 26
				.Width = 0
				.Visible = .t.&&
			ENDWITH			
			lnCnt = lnCnt + 1
		ENDFOR
 	ENDPROC
 	
 	PROCEDURE Container1.Resize()
 	LOCAL lnPanelLeft,lnPanelTop
		lnPanelTop = 4
		&&
		THIS.Command1.Left = 19 && выбрать всё/отменить
		lnPanelLeft = THIS.Command1.Left + THIS.Command1.Width
		THIS.Command1.Top = 4
		&&
		THIS.Line1.Left = lnPanelLeft + 4 
		lnPanelLeft = THIS.Line1.Left + THIS.Line1.Width
		THIS.Line1.Top = lnPanelTop
 	ENDPROC 		
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: RContainerEvent - класс для контейнера формы ReqForm
DEFINE CLASS RContainerEvent AS Custom
	&& Command1
	PROCEDURE Command1Click()
	LOCAL lcForm, ;
	lnWorkArea
		lcForm = oModel.FindForm("RequisitesForm")
		IF VARTYPE(lcForm) == "O"
			lnWorkArea = oModel.IsUsed("rqcursor")
			IF lnWorkArea > 0 &&upd
				SELECT (lnWorkArea)
				IF RECCOUNT(ALIAS()) > 0&&
					&& commandbutton = f11
					IF oPath.tbF11Press
						oPath.SetMyF11F12(.f.,.f.,"rqcursor",2)
					ELSE
						oPath.SetMyF11F12(.t.,.t.,"rqcursor",2)
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDPROC
		
	PROCEDURE Command1Error()
	LPARAMETERS nError, cMethod, nLine
	 	SELECT 1
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: IzmForm - класс формы IzmForm
DEFINE CLASS IzmForm AS Form
	BackColor = RGB(219,215,210)
	BorderStyle = 3
	Caption = ""
	Closable = .t.
	Height = SYSMETRIC(22)
	KeyPreview = .t.
	Left = 23
	MaxButton = .t.
	ScrollBars = 0
	ShowInTaskBar = .t.
	ShowTips = .t.
	ShowWindow = 1
	Top = 23
	Visible = .f.
	Width = SYSMETRIC(21)
	WindowState = 2
	WindowType = 0
	
	DIMENSION taIndexColumn(9), ;
			  taFForm(10), ;
		      taPriz(1)	      		     
	&& Копия параметров формы
	tnKeyCnt = 0
	tnId = 0 && Номер операции со строкой в бэкап-таблице
	tnRecno = 0
	tnColIndex = 0
	tnSortNumber = 1
	tnCombo1 = 0
	tnPressSearch = 0
	tnClickSearch = 0
	tcPre = "Корректура" && название формы
	tcAlias = "ccursor"
	tlSearch = .f.
	
	PROCEDURE MyBindEvent()
	PARAMETERS tnGridCnt
	LOCAL lcFormT
		FOR EACH myformobject IN THISFORM.Objects
			IF AT(UPPER("grid"),UPPER(myformobject.name)) > 0
				&& выбрали grid
				IF myformobject.TabIndex = tnGridCnt				
					FOR EACH mycolumn IN myformobject.Objects
						FOR EACH mygridobject IN mycolumn.Objects						
							IF ALLTRIM(UPPER(mygridobject.Name)) == UPPER('text1')
								BINDEVENT(mygridobject,"Click",myformobject,"Click")
								BINDEVENT(mygridobject,"KeyPress",myformobject,"KeyPress")
								BINDEVENT(mygridobject,"RightClick",myformobject,"RightClick")
								BINDEVENT(mygridobject,"MouseUp",myformobject,"MouseUp")
							ENDIF
							&&
							IF ALLTRIM(UPPER(mygridobject.Name)) == UPPER('header1')
								IF tnGridCnt = 1
									&&grid1
									FOR EACH mainobject IN Application.Objects
										IF ALLTRIM(UPPER(mainobject.Name)) == UPPER('columnheader')
											BINDEVENT(mygridobject,"Click",oColumnHeader,"Click")
										ENDIF
									ENDFOR
								ENDIF
							ENDIF
						ENDFOR
					ENDFOR
				ENDIF
			ENDIF
		ENDFOR
	ENDPROC
	
	PROCEDURE MyAfterRowColChange()
	ENDPROC
	
	PROCEDURE Init()
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.HighlightStyle = 2
				myobject.HighlightBackColor = oPath.tnGridColor
				myobject.HighlightForeColor = oPath.tnGridTextColor
			ENDIF
		ENDFOR
		&&
		THISFORM.Refresh()
	ENDPROC
	
	PROCEDURE Resize()
	LOCAL lcForm, lcAliasname, ;
	lnPanelLeft, lnPanelTop
		IF THISFORM.WindowState = 0
			&& n
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0					
					myobject.Width = THISFORM.Width
					myobject.Height = THISFORM.Height - 34
				ENDIF
			ENDFOR
		ENDIF
		
		IF THISFORM.WindowState = 2
			&& max
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0			
					myobject.Width = THISFORM.Width + 2
					myobject.Height = THISFORM.Height - 33
				ENDIF
			ENDFOR
		ENDIF
		
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0
				FOR EACH mycontainer IN myobject.Objects
					IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
						myobject.Top = 34
					ENDIF
				ENDFOR
			ENDIF
		ENDFOR
		&&
		*FOR EACH myobject IN THISFORM.Objects
		*	IF AT(UPPER("container"),ALLTRIM(UPPER(myobject.Name))) > 0
*!*					myobject.Left = 0
*!*					myobject.Top = 0
*!*					myobject.Width = THISFORM.Width
*!*					myobject.Height = 34
*!*					myobject.Resize()
*!*				ENDIF
*!*			ENDFOR
				THISFORM.Container1.Left = 0
				THISFORM.Container1.Top = 0
				THISFORM.Container1.Width = THISFORM.Width
				THISFORM.Container1.Height = 34
				THISFORM.Container1.Resize()		
		&& m10870
		oPath.SetCustomFont()
	ENDPROC		

	PROCEDURE QueryUnload()
		oPath.MarkMenu()
		FormDelete("izmform")				
		THISFORM.Hide()
		THISFORM.Destroy()
	ENDPROC
	
	PROCEDURE Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& Grid1
	ADD OBJECT Grid1 AS Grid WITH Name = "Grid1", ;
	AllowCellSelection = .t., BackColor = RGB(248,244,255), ColumnCount = -1, ;
	DeleteMark = .f., Height = 428, HighlightStyle = 2, Left = 0, ReadOnly = .t., ;
	RecordSourceType = 1, RowHeight = 21, ScrollBars = 3, Top = 48, View = 0, Visible = .t., Width = 640
 	
 	PROCEDURE Grid1.MyObject()
 		RETURN THIS
 	ENDPROC
 	
 	PROCEDURE Grid1.MyKeyPress()
 	PARAMETERS tnKeyCode
 	LOCAL lnAnswer
		lnAnswer = tnKeyCode&&LASTKEY()
		&&
		DO CASE
			CASE lnAnswer = 1
				&& ctrl+a
				oCell.GridSelectAll()
				
			CASE lnAnswer = 3
				&& ctrl+c
				oCell.GridCopyOption()
		ENDCASE
 	ENDPROC
 	
 	PROCEDURE Grid1.GetMouseObject()
 	LPARAMETERS tcClass	
	LOCAL loRet
	&& Получение ссылки на объект под курсором
		IF EMPTY(tcClass)
			tcClass = "Column"
		ENDIF

		LOCAL ARRAY laM[2]
		AMOUSEOBJ(laM, 1)
		IF TYPE("laM[1]") = "O" 
			IF laM[1].BaseClass = tcClass
				loRet = laM[1]
			ELSE
				IF TYPE("laM[1].ControlCount") = "N"
					FOR i = 1 TO laM[1].ControlCount
						IF laM[1].Controls(i).BaseClass = tcClass
							loRet = laM[1].Controls(i)
							EXIT 
						ENDIF 
					ENDFOR 
				ENDIF 
			ENDIF 
		ENDIF 
		&&
		RETURN loRet
 	ENDPROC
 
 	PROCEDURE Grid1.Init()
 		THIS.Refresh()
 	ENDPROC
 	
 	PROCEDURE Grid1.Click()
		&& Сохраняем номер выбранной строки для поиска по связанным таблицам
		THISFORM.tlSearch = .f.
		&&		
		SELECT ALIAS()
		THISFORM.tnRecno = RECNO(ALIAS())
		GOTO (THISFORM.tnRecno) IN ALIAS()
 	ENDPROC
 	
 	PROCEDURE Grid1.KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lnAnswer
		lnAnswer = LASTKEY()
		SELECT ALIAS()
		THISFORM.tnRecno = RECNO(ALIAS())
				
		DO CASE					
			CASE lnAnswer = 1 
				&& HOME
				IF !THISFORM.tlSearch
					GO TOP IN ALIAS()
					THIS.SetFocus()
				ENDIF

			CASE lnAnswer = 6
				&& END
				IF !THISFORM.tlSearch
					GO BOTTOM IN ALIAS()
					THIS.SetFocus()
				ENDIF
		ENDCASE	
	ENDPROC
 
 	PROCEDURE Grid1.AfterRowColChange()
 	LPARAMETERS nColIndex
		&& Номер элемента массива, который хранит дескриптор формы
		THISFORM.tnColIndex = nColIndex
 	ENDPROC
 	
 	PROCEDURE Grid1.MouseUp()
 	LPARAMETERS nButton, nShift, nXCoord, nYCoord
 		IF nButton = 2
			*oModel.DoContext()
		ENDIF
 	ENDPROC
 	
 	PROCEDURE Grid1.Error()
 	LPARAMETERS nError, cMethod, nLine
		*SELECT 1
 	ENDPROC

 	&& Container1
 	ADD OBJECT Container1 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .t.

	PROCEDURE Container1.AddMyItem()
		&& delete all
		FOR i = 1 TO THIS.Combo1.ListCount
			THIS.Combo1.RemoveListItem(i)
		ENDFOR
				
		&& add
		THIS.Combo1.AddItem("№ изделия") && 1
		THIS.Combo1.AddItem("Цех+№ изделия") && 2
		THIS.Combo1.AddItem("Код+№ изделия") && 3
		THIS.Combo1.AddItem("№ изд.(ст.)") && 4
		THIS.Combo1.AddItem("ДСЕ (внешний)") && 5
		THIS.Combo1.AddItem("ДСЕ (внутренний)") && 6
		THIS.Combo1.AddItem("Наименование")&& 7
		THIS.Combo1.AddItem("Признак") && 8
		THIS.Combo1.AddItem("Без индекса") && 9
		&&
		DIMENSION THISFORM.taIndexColumn(9)
		THISFORM.taIndexColumn[1] = 2
		THISFORM.taIndexColumn[2] = 3
		THISFORM.taIndexColumn[3] = 10
		THISFORM.taIndexColumn[4] = 6
		THISFORM.taIndexColumn[5] = 7
		THISFORM.taIndexColumn[6] = 1
		THISFORM.taIndexColumn[7] = 8
		THISFORM.taIndexColumn[8] = 11
		THISFORM.taIndexColumn[9] = 0
		
		THIS.Combo1.Value = 1
		THISFORM.tnCombo1 = THIS.Combo1.Value
		THIS.Combo1.DisplayCount = THIS.Combo1.ListCount
	ENDPROC

	PROCEDURE Container1.Init()
		&& Combo1
		THIS.AddObject("Combo1","ComboBox")
		WITH THIS.Combo1
			.Enabled = .t.
			.Height = 26
			.ReadOnly = .f.
			.Style = 2
			.ToolTipText = "Индекс сортировки"
			.Visible = .t.
			.Width = 175
		ENDWITH
		&&
		BINDEVENT(THIS.Combo1,"Click",oIContainerEvent,"Combo1Click")
		BINDEVENT(THIS.Combo1,"Error",oIContainerEvent,"Combo1Error")
		BINDEVENT(THIS.Combo1,"MouseMove",oIContainerEvent,"Combo1MouseMove")
*!*				
		&& Text1
		THIS.AddObject("Text1","TextBox")
		WITH THIS.Text1 
			.Alignment = 0
			.BackColor = RGB(248,244,255)
			.Enabled = .t.
			.FontName = 'Consolas'
			.FontSize = 11
			.Height = 26
			.ToolTipText = "Укажите, что искать (Ctrl+F)"
			.Value = "Поиск"
			.Visible = .t.
			.Width = 202
			&&
			PUBLIC gbTextMenu1
			gbTextMenu1 = .f.
		ENDWITH
		&&
		BINDEVENT(THIS.Text1,"Click",oIContainerEvent,"Text1Click")
		BINDEVENT(THIS.Text1,"KeyPress",oIContainerEvent,"Text1KeyPress")
		BINDEVENT(THIS.Text1,"Error",oIContainerEvent,"Text1Error")
		BINDEVENT(THIS.Text1,"LostFocus",oIContainerEvent,"Text1LostFocus")
		BINDEVENT(THIS.Text1,"MouseUp",oIContainerEvent,"Text1MouseUp")
		
		&& Command1
		THIS.AddObject("Command1","CommandButton")
		WITH THIS.Command1
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\search.png"
			.PictureMargin = 0
			.PicturePosition = 14
			.ToolTipText = "Поиск по таблице (Enter)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command1,"Click",oIContainerEvent,"Command1Click")
		BINDEVENT(THIS.Command1,"Error",oIContainerEvent,"Command1Error")				
					
		&& Line
		LOCAL lnCnt,loTemp
		lnCnt = 1
		FOR i = 1 TO 3&&5
			THIS.AddObject("Line"+ALLTRIM(STR(lnCnt)),"Line")
			loTemp = GETPEM(THIS,"Line"+ALLTRIM(STR(lnCnt)))
			WITH loTemp
				.Height = 26
				.Width = 0
				.Visible = .t.
			ENDWITH			
			lnCnt = lnCnt + 1
		ENDFOR
		&&
		THIS.AddMyItem()
	ENDPROC
	
	PROCEDURE Container1.Resize()
	LOCAL lnPanelLeft,lnPanelTop
		lnPanelTop = 4
		&&
		THIS.Combo1.Left = 19
		lnPanelLeft = THIS.Combo1.Left + THIS.Combo1.Width
		THIS.Combo1.Top = 4
		&&
		THIS.Line1.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Line1.Left + THIS.Line1.Width
		THIS.Line1.Top = lnPanelTop
		&&
		THIS.Text1.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Text1.Left + THIS.Text1.Width
		THIS.Text1.Top = lnPanelTop
		&&
		THIS.Line2.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Line2.Left + THIS.Line2.Width
		THIS.Line2.Top = lnPanelTop
		&&
		THIS.Command1.Left = lnPanelLeft + 5 && Поиск
		lnPanelLeft = THIS.Command1.Left + THIS.Command1.Width
		THIS.Command1.Top = lnPanelTop
		&&
		THIS.Line3.Left = lnPanelLeft + 4 
		lnPanelLeft = THIS.Line3.Left + THIS.Line3.Width
		THIS.Line3.Top = lnPanelTop
		&&
		THIS.Command2.Left = lnPanelLeft + 5 && Отменить изменения
		lnPanelLeft = THIS.Command2.Left + THIS.Command2.Width
		THIS.Command2.Top = lnPanelTop
		&&
		THIS.Line4.Left = lnPanelLeft + 4
		lnPanelLeft = THIS.Line4.Left + THIS.Line4.Width
		THIS.Line4.Top = lnPanelTop
		
		THIS.Command3.Left = lnPanelLeft + 5 && Добавить строку
		lnPanelLeft = THIS.Command3.Left + THIS.Command3.Width
		THIS.Command3.Top = lnPanelTop
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: IContainerEvent - класс для контейнера формы IzmForm
DEFINE CLASS IContainerEvent AS Custom
	PROCEDURE Combo1Click()
	LOCAL lcForm, lcTxt
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"
			IF lcForm.Grid1.Visible
				SELECT "ccursor"
				SCATTER MEMVAR
				&&
				lcTxt = TRIM(m.izm) + " " + ;				
					TRIM(m.nizd) + " " + ;
					TRIM(m.snizd) + " " + ;
					TRIM(m.mod) + " " + ;
					TRIM(m.kudar) + " " + ;
					TRIM(m.kuda) + " " + ;
					TRIM(DTOC(m.dtv)) + " " + ;
					TRIM(m.cex) + " " + ;
					TRIM(m.rank) + " " + ;
					TRIM(m.kd) + " " + ;
					TRIM(m.priz) + " " + ;
					TRIM(m.naim)
				&&
				oModel_izm.SearchRecord(lcTxt, lcForm.Container1.Combo1.ListIndex) &&THIS
				lnRecno = lcForm.tnRecno
				GOTO (lnRecno) IN "ccursor"
				lnColIndex = lcForm.tnColIndex
				lcForm.tnCombo1 = lcForm.Container1.Combo1.ListIndex
				lcForm.Grid1.Columns(lnColIndex).Text1.SetFocus()
				lcForm.Grid1.SetFocus()
			ENDIF
		ENDIF	
	ENDPROC
	
	PROCEDURE Combo1MouseMove()
	LPARAMETERS nButton, nShift, nXCoord, nYCoord
		NODEFAULT
	ENDPROC
	
	PROCEDURE Combo1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	PROCEDURE Text1Click()
	LOCAL lcForm		
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.
			IF lcForm.tnClickSearch = 0
				lcForm.Container1.Text1.Value = ""
				lcForm.tnPressSearch = lcForm.tnPressSearch + 1
				lcForm.tnClickSearch = lcForm.tnClickSearch + 1
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE Text1KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lcForm	
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.
			IF lcForm.tnPressSearch = 0	
				lcForm.Container1.Text1.Value = ""
				lcForm.tnPressSearch = lcForm.tnPressSearch + 1
				lcForm.tnClickSearch = lcForm.tnClickSearch + 1
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE Text1MyKeyPress()
	PARAMETERS tnKeyCode
	LOCAL lcForm,lcTemp,lcSymbol, ;
	lnSelStart,lnSelLength, ;
	loNode
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"
			loNode = oModel.MyObject()
			lnSelStart = loNode.SelStart
			lnSelLength = loNode.SelLength
			&&
			DO CASE 
				CASE tnKeyCode = 1
					&& ctrl+a
					oCell.TextSelectAll()
					
				CASE tnKeyCode = 3
					&& ctrl+c
					gbTextMenu1 = .t.
					oCell.TextCopyOption()
					lcForm.Container1.Text1.SetFocus()
			ENDCASE			
		ENDIF
	ENDPROC
	
	PROCEDURE Text1MouseUp()
 	LPARAMETERS nButton, nShift, nXCoord, nYCoord
 	LOCAL lcForm
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.			
			IF nButton = 2
	 			IF lcForm.tnClickSearch = 0
					lcForm.Container1.Text1.Value = ""
					lcForm.tnPressSearch = lcForm.tnPressSearch + 1
					lcForm.tnClickSearch = lcForm.tnClickSearch + 1
				ENDIF
				oModel.DoText()
			ENDIF
		ENDIF	
 	ENDPROC
	
	PROCEDURE Text1LostFocus()
	LOCAL lcForm
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"	
			IF TYPE("gbTextMenu1") != "U"
				IF gbTextMenu1
					&& чтобы фокус не уходил из text1
					gbTextMenu1 = .f.
					NODEFAULT			
					lcForm.Container1.Text1.SetFocus()
				ENDIF
			ENDIF
		ENDIF	
	ENDPROC
	
	PROCEDURE Text1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& command1 - поиск по таблице
	PROCEDURE Command1Click() 
	LOCAL lcForm, lcValue, lcChoiceSearch
		lcForm = oModel.FindForm("IzmForm")
		IF VARTYPE(lcForm) == "O"		
			lcValue = ALLTRIM(UPPER(lcForm.Container1.Text1.Value))
			IF lcForm.Grid1.Visible
				IF !EMPTY(lcValue)		
					DO CASE 			
						CASE lcForm.Container1.Combo1.ListIndex = 1		
							&& nizd
							oModel_izm.Search(2,lcValue)

						CASE lcForm.Container1.Combo1.ListIndex = 2
							&& cn
							oModel_izm.Search(3,lcValue)
						
						CASE lcForm.Container1.Combo1.ListIndex = 3
							&& kdn
							oModel_izm.Search(10,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 4
							&& snizd
							oModel_izm.Search(6,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 5
							&& kdr
							oModel_izm.Search(7,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 6
							&& kd
							oModel_izm.Search(1,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 7
							&& naim
							oModel_izm.Search(8,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 8
							&& priz
							oModel_izm.Search(11,lcValue)
							
						CASE lcForm.Container1.Combo1.ListIndex = 9
							&& no
							oModel_izm.Search(0,lcValue)
					ENDCASE
				ENDIF
				&&
				lcForm.Grid1.Refresh()
			ENDIF			
		ENDIF
	ENDPROC
			
	PROCEDURE Command1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: PrizForm - класс формы PrizForm
DEFINE CLASS PrizForm AS Form
	AlwaysOnTop = .t.
	BackColor = RGB(219,215,210)
	BorderStyle = 3
	Left = 1
	MaxButton = .t.&&f
	ShowTips = .t.
	Top = 1	
	Visible = .f.
	ShowWindow = 0
	
	DIMENSION taPriz(1), ;
		taPrizAnswer(1) 
	tnColIndex = 1
	tcPre = "Признак"
	
	PROCEDURE MyBindEvent()
	PARAMETERS tnGridCnt
	LOCAL lcFormT
		FOR EACH myformobject IN THISFORM.Objects
			IF AT(UPPER("grid"),UPPER(myformobject.name)) > 0
				&& выбрали grid
				IF myformobject.TabIndex = tnGridCnt				
					FOR EACH mycolumn IN myformobject.Objects
						FOR EACH mygridobject IN mycolumn.Objects						
							IF ALLTRIM(UPPER(mygridobject.Name)) == UPPER('text1')
								BINDEVENT(mygridobject,"Click",myformobject,"Click")
								BINDEVENT(mygridobject,"KeyPress",myformobject,"KeyPress")
								BINDEVENT(mygridobject,"RightClick",myformobject,"RightClick")
								BINDEVENT(mygridobject,"MouseUp",myformobject,"MouseUp")
							ENDIF							
						ENDFOR
					ENDFOR
				ENDIF
			ENDIF
		ENDFOR
	ENDPROC
	
	PROCEDURE MyAfterRowColChange()
	ENDPROC
	
	PROCEDURE Init()
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.HighlightStyle = 2
				myobject.HighlightBackColor = oPath.tnGridColor
				myobject.HighlightForeColor = oPath.tnGridTextColor
			ENDIF
		ENDFOR
		&&		
		THISFORM.Width = oPath.tnPrizFormWidth&&upd
		THISFORM.Height = oPath.tnPrizFormHeight
		THISFORM.Refresh()
	ENDPROC
	
	PROCEDURE Resize()
	LOCAL lcForm, lcAliasname, ;
	lnPanelLeft, lnPanelTop											
		IF THISFORM.WindowState = 0
			&& n
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0
					IF THISFORM.Width != oPath.tnPrizFormWidth&&upd
						oPath.SetMyParam("tnPrizFormWidth",THISFORM.Width)
					ENDIF
					
					IF THISFORM.Height != oPath.tnPrizFormHeight&&upd
						oPath.SetMyParam("tnPrizFormHeight",THISFORM.Height)
					ENDIF
																
					myobject.Width = THISFORM.Width
					myobject.Height = THISFORM.Height&& - 34
				ENDIF
			ENDFOR
		ENDIF
		
		IF THISFORM.WindowState = 2
			&& max
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0			
					myobject.Width = THISFORM.Width + 2
					myobject.Height = THISFORM.Height - 33
				ENDIF
			ENDFOR
		ENDIF
		
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0
				FOR EACH mycontainer IN myobject.Objects
					IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
						myobject.Top = 34
					ENDIF
				ENDFOR
			ENDIF
		ENDFOR
		&&
		THISFORM.Container1.Left = 0
		THISFORM.Container1.Top = 0
		THISFORM.Container1.Width = THISFORM.Width
		THISFORM.Container1.Height = 34
		THISFORM.Container1.Resize()		
		&& m10870
		oPath.SetCustomFont()
	ENDPROC		

	PROCEDURE QueryUnload()
		oModel_priz.SetFilter()&&upd
		oPath.MarkMenu()
		FormDelete("prizform")				
		THISFORM.Hide()
		THISFORM.Destroy()
	ENDPROC
	
	PROCEDURE Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& Grid1
	ADD OBJECT Grid1 AS Grid WITH Name = "Grid1", ;
	AllowCellSelection = .t., BackColor = RGB(248,244,255), ColumnCount = -1, ;
	DeleteMark = .f., Height = 428, HighlightStyle = 2, Left = 0, ReadOnly = .f., ;
	RecordSourceType = 1, RowHeight = 21, ScrollBars = 3, Top = 48, View = 0, Visible = .t., Width = 640
 	
 	PROCEDURE Grid1.MyObject()
 		RETURN THIS
 	ENDPROC
 	
 	PROCEDURE Grid1.MyKeyPress()
 	PARAMETERS tnKeyCode
 	LOCAL lnAnswer
		lnAnswer = tnKeyCode&&LASTKEY()
		&&
		DO CASE
			CASE lnAnswer = 1
				&& ctrl+a
				oCell.GridSelectAll()
				
			CASE lnAnswer = 3
				&& ctrl+c
				oCell.GridCopyOption()
		ENDCASE
 	ENDPROC
 	
 	PROCEDURE Grid1.GetMouseObject()
 	LPARAMETERS tcClass	
	LOCAL loRet
	&& Получение ссылки на объект под курсором
		IF EMPTY(tcClass)
			tcClass = "Column"
		ENDIF

		LOCAL ARRAY laM[2]
		AMOUSEOBJ(laM, 1)
		IF TYPE("laM[1]") = "O" 
			IF laM[1].BaseClass = tcClass
				loRet = laM[1]
			ELSE
				IF TYPE("laM[1].ControlCount") = "N"
					FOR i = 1 TO laM[1].ControlCount
						IF laM[1].Controls(i).BaseClass = tcClass
							loRet = laM[1].Controls(i)
							EXIT 
						ENDIF 
					ENDFOR 
				ENDIF 
			ENDIF 
		ENDIF 
		&&
		RETURN loRet
 	ENDPROC
 
 	PROCEDURE Grid1.Init()
 		THIS.Refresh()
 	ENDPROC
 	
 	PROCEDURE Grid1.Click()	
	LOCAL lnColIndex,lnWorkArea	
		lnColIndex = THISFORM.tnColIndex
		&&
		IF lnColIndex = 1
			lnWorkArea = oModel.IsUsed("pcursor")
			IF lnWorkArea > 0
				LOCAL lcForm
				FOR EACH myobject IN THISFORM.Controls
					IF UPPER(ALLTRIM(myobject.BaseClass)) == UPPER("grid")
						lcForm = myobject
						EXIT
					ENDIF
				ENDFOR

				IF VARTYPE(lcForm) == "O"
					SELECT (lnWorkArea)
					LOCAL lcAnswer
					IF UPPER(ALLTRIM(fl)) = UPPER("*")
						&& да->нет
						lcAnswer = ""						
					ELSE 
						IF UPPER(ALLTRIM(fl)) = UPPER("")
							&& нет->да
							lcAnswer = "*"
						ENDIF					
					ENDIF 
					&&
					REPLACE fl WITH (lcAnswer)
					lcForm.Columns(lnColIndex).Text1.Value = (lcAnswer)
					&& изменение среди реквизитов, которые отображаем
					LOCAL lcAnswer,lnRecno
					lcAnswer = ""
					lnRecno = RECNO(lnWorkArea)
					SCAN
						lcAnswer = lcAnswer + ALLTRIM(fl) + ","
					ENDSCAN
					&&
					oPath.tcPrizAnswer = lcAnswer
					oPath.SetMyParam("tcPrizAnswer",oPath.tcPrizAnswer)
					GOTO (lnRecno) IN (lnWorkArea)
				ENDIF
			ENDIF			
		ENDIF
 	ENDPROC
 	
 	PROCEDURE Grid1.KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lnAnswer
		lnAnswer = LASTKEY()
		SELECT ALIAS()
		THISFORM.tnRecno = RECNO(ALIAS())
				
		DO CASE					
			CASE lnAnswer = 1 
				&& HOME
				IF !THISFORM.tlSearch
					GO TOP IN ALIAS()
					THIS.SetFocus()
				ENDIF

			CASE lnAnswer = 6
				&& END
				IF !THISFORM.tlSearch
					GO BOTTOM IN ALIAS()
					THIS.SetFocus()
				ENDIF
		ENDCASE	
	ENDPROC
 
 	PROCEDURE Grid1.AfterRowColChange()
 	LPARAMETERS nColIndex
		&& Номер элемента массива, который хранит дескриптор формы
		THISFORM.tnColIndex = nColIndex
 	ENDPROC
 	
 	PROCEDURE Grid1.MouseUp()
 	LPARAMETERS nButton, nShift, nXCoord, nYCoord
 		IF nButton = 2
			*oModel.DoContext()
		ENDIF
 	ENDPROC
 	
 	PROCEDURE Grid1.Error()
 	LPARAMETERS nError, cMethod, nLine
		*SELECT 1
 	ENDPROC

 	&& Container1
 	ADD OBJECT Container1 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .t.

	PROCEDURE Container1.Init()
		&& Command1
		THIS.AddObject("Command1","CommandButton")
		WITH THIS.Command1
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\asterisk.png"
			.PictureMargin = 0
			.PicturePosition = 14
			.ToolTipText = "Выбрать все/Отменить выбор (F11/F12)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command1,"Click",oPContainerEvent,"Command1Click")
		BINDEVENT(THIS.Command1,"Error",oPContainerEvent,"Command1Error")				
					
		&& Line
		LOCAL lnCnt,loTemp
		lnCnt = 1
		FOR i = 1 TO 1&&5
			THIS.AddObject("Line"+ALLTRIM(STR(lnCnt)),"Line")
			loTemp = GETPEM(THIS,"Line"+ALLTRIM(STR(lnCnt)))
			WITH loTemp
				.Height = 26
				.Width = 0
				.Visible = .t.
			ENDWITH			
			lnCnt = lnCnt + 1
		ENDFOR
	ENDPROC
	
	PROCEDURE Container1.Resize()
	LOCAL lnPanelLeft,lnPanelTop
		lnPanelTop = 4
		&&
		THIS.Command1.Left = 19 && Выбрать всё/Отменить
		lnPanelLeft = THIS.Command1.Left + THIS.Command1.Width
		THIS.Command1.Top = lnPanelTop
		&&
		THIS.Line1.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Line1.Left + THIS.Line1.Width
		THIS.Line1.Top = lnPanelTop
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: PContainerEvent - класс для контейнера формы IzmForm
DEFINE CLASS PContainerEvent AS Custom
	&& command1 - выбрать всё/отменить
	PROCEDURE Command1Click()
	LOCAL lcForm, ;
	lnWorkArea
		lcForm = oModel.FindForm("PrizForm")
		IF VARTYPE(lcForm) == "O"
			lnWorkArea = oModel.IsUsed("pcursor")
			IF lnWorkArea > 0 &&upd
				SELECT (lnWorkArea)
				IF RECCOUNT(ALIAS()) > 0&&
					&& commandbutton = f11
					IF oPath.tbF11Press
						oPath.SetMyF11F12(.f.,.f.,"pcursor",1)
					ELSE
						oPath.SetMyF11F12(.t.,.t.,"pcursor",1)
					ENDIF
					
					&& изменение среди реквизитов, которые отображаем
					LOCAL lcAnswer,lnRecno&&upd
					lcAnswer = ""
					lnRecno = RECNO(lnWorkArea)
					LOCAL lnCnt
					lnCnt = 0
					&&
					SCAN					
						lcAnswer = lcAnswer + ALLTRIM(fl) + ","
					ENDSCAN

					oPath.tcPrizAnswer = lcAnswer
					oPath.SetMyParam("tcPrizAnswer",oPath.tcPrizAnswer)
					GOTO (lnRecno) IN (lnWorkArea)
				ENDIF
			ENDIF
		ENDIF 
	ENDPROC
			
	PROCEDURE Command1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: CheckKdForm - класс формы CheckKdForm
DEFINE CLASS CheckKdForm AS Form
	AlwaysOnTop = .t.
	BackColor = RGB(219,215,210)
	BorderStyle = 1
	Left = 1
	MaxButton = .f.
	ShowTips = .t.
	Top = 1	
	Visible = .f.
	ShowWindow = 0
	
	tcPre = "Проверить код"
	tnRecno = 1
	
	PROCEDURE Init()
		THISFORM.Refresh()
	ENDPROC
	
	PROCEDURE Resize()	
		THISFORM.Container1.Left = 0
		THISFORM.Container1.Top = 0
		THISFORM.Container1.Height = 34
		THISFORM.Container1.Resize()
		&&
		THISFORM.Width = THISFORM.Container1.Width
		THISFORM.Height = THISFORM.Container1.Height		
		&& m10870
		oPath.SetCustomFont()
	ENDPROC		

	PROCEDURE QueryUnload()
		oPath.MarkMenu()
		FormDelete("selectkdform")
		THISFORM.Hide()
		THISFORM.Destroy()
	ENDPROC
	
	PROCEDURE Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& Container1
 	ADD OBJECT Container1 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .t.

	PROCEDURE Container1.Init()
		&& Command1
		THIS.AddObject("Command1","CommandButton")
		WITH THIS.Command1
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\search.png"
			.PictureMargin = 0
			.PicturePosition = 14
			.ToolTipText = "Поиск по таблице (Enter)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command1,"Click",oCKContainerEvent,"Command1Click")
		BINDEVENT(THIS.Command1,"Error",oCKContainerEvent,"Command1Error")				
					
		&& Text1
		THIS.AddObject("Text1","TextBox")
		WITH THIS.Text1 
			.Alignment = 0
			.BackColor = RGB(248,244,255)
			.Enabled = .t.
			.FontName = 'Consolas'
			.FontSize = 11
			.Height = 26
			.ReadOnly = .t.
			.ToolTipText = "Номер строки"
			.Value = "Поиск"
			.Visible = .t.
			.Width = 202			
		ENDWITH
		&&
		BINDEVENT(THIS.Text1,"Error",oCKContainerEvent,"Text1Error")
							
		&& Line
		LOCAL lnCnt,loTemp
		lnCnt = 1
		FOR i = 1 TO 1&&5
			THIS.AddObject("Line"+ALLTRIM(STR(lnCnt)),"Line")
			loTemp = GETPEM(THIS,"Line"+ALLTRIM(STR(lnCnt)))
			WITH loTemp
				.Height = 26
				.Width = 0
				.Visible = .t.
			ENDWITH			
			lnCnt = lnCnt + 1
		ENDFOR
	ENDPROC
	
	PROCEDURE Container1.Resize()
	LOCAL lnPanelLeft,lnPanelTop
		lnPanelTop = 4
		&&
		THIS.Text1.Left = 19 && Выбрать всё/Отменить
		lnPanelLeft = THIS.Text1.Left + THIS.Text1.Width
		THIS.Text1.Top = lnPanelTop
		&&
		THIS.Line1.Left = lnPanelLeft + 4
		lnPanelLeft = THIS.Line1.Left + THIS.Line1.Width
		THIS.Line1.Top = lnPanelTop
		&&
		THIS.Command1.Left = lnPanelLeft + 5 && Поиск по таблице
		lnPanelLeft = THIS.Command1.Left + THIS.Command1.Width
		THIS.Command1.Top = lnPanelTop
		&&
		THIS.Width = THIS.Command1.Left + THIS.Command1.Width + THIS.Text1.Left
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: CKContainerEvent - класс для контейнера формы CheckKdForm
DEFINE CLASS CKContainerEvent AS Custom
	&& command1 - поиск по таблице
	PROCEDURE Command1Click()
	LOCAL lcForm,lcFormT
		lcForm = oModel.FindForm("CheckKdForm")
		lcFormT = oModel.FindForm("CustomForm")
		IF VARTYPE(lcForm) == "O"
			LOCAL lnWorkArea,lnWorkAreaT,lnFind
			lnWorkArea = oModel.IsUsed("rtable")
			lnWorkAreaT = oModel.IsUsed("kdcursor")
			IF lnWorkArea > 0 AND lnWorkAreaT > 0
				SELECT (lnWorkAreaT)
				DO WHILE !EOF(lnWorkAreaT)
					SELECT (lnWorkArea)
					GOTO kdcursor.myrecno IN (lnWorkArea)
					lcForm.Container1.Text1.Value = "Строка " + ALLTRIM(STR(kdcursor.myrecno))
					SKIP IN (lnWorkAreaT)
					EXIT
				ENDDO
				
				IF EOF(lnWorkAreaT)
					GO TOP IN (lnWorkAreaT)
				ENDIF
				&&
				lcFormT.Grid1.Column10.SetFocus() &&kd
			ENDIF
		ENDIF
	ENDPROC 
	
	PROCEDURE Command1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	&& text1
	PROCEDURE Text1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC		
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: SelectForm - класс формы SelectForm
DEFINE CLASS SelectForm AS Form
	BackColor = RGB(219,215,210)
	BorderStyle = 3
	Caption = ""
	Closable = .t.
	Height = SYSMETRIC(22)
	KeyPreview = .t.
	Left = 23
	MaxButton = .t.
	ScrollBars = 0
	ShowInTaskBar = .t.
	ShowTips = .t.
	ShowWindow = 1
	Top = 23
	Visible = .f.
	Width = SYSMETRIC(21)
	WindowState = 0
	WindowType = 0
	
	DIMENSION taIndexColumn(9), ;
			  taFForm(10)
	&& Копия параметров формы
	tnKeyCnt = 0
	tnId = 0 && Номер операции со строкой в бэкап-таблице
	tnRecno = 0
	tnColIndex = 1
	tnSortNumber = 1
	tnCombo1 = 0
	tnPressSearch = 0
	tnClickSearch = 0
	tcPre = "Выбрать код" && название формы
	tcAlias = ""
	tcTime = .f.&&upd
	tdDate = .f.&&upd
	tlSearch = .f.
			
	PROCEDURE MyBindEvent()
	PARAMETERS tnGridCnt
	LOCAL lcFormT
		FOR EACH myformobject IN THISFORM.Objects
			IF AT(UPPER("grid"),UPPER(myformobject.name)) > 0
				&& выбрали grid
				IF myformobject.TabIndex = tnGridCnt				
					FOR EACH mycolumn IN myformobject.Objects
						FOR EACH mygridobject IN mycolumn.Objects						
							IF ALLTRIM(UPPER(mygridobject.Name)) == UPPER('text1')
								BINDEVENT(mygridobject,"Click",myformobject,"Click")
								BINDEVENT(mygridobject,"KeyPress",myformobject,"KeyPress")
							ENDIF
							&&
							IF ALLTRIM(UPPER(mygridobject.Name)) == UPPER('header1')
								IF tnGridCnt = 1
									&&grid1
									FOR EACH mainobject IN Application.Objects
										IF ALLTRIM(UPPER(mainobject.Name)) == UPPER('columnheader')
											BINDEVENT(mygridobject,"Click",oColumnHeader,"Click")
										ENDIF
									ENDFOR
								ENDIF
							ENDIF
						ENDFOR
					ENDFOR
				ENDIF
			ENDIF
		ENDFOR
	ENDPROC
	
	PROCEDURE FormSettings()
	PARAMETERS tnMode
		IF tnMode = 1
			THISFORM.Grid1.Visible = .t.
			THISFORM.Grid2.Visible = .f.
			THISFORM.Container1.Visible = .t.				
			THISFORM.Container2.Visible = .f.
		ELSE
			THISFORM.Grid1.Visible = .f.
			THISFORM.Grid2.Visible = .t.
			THISFORM.Container1.Visible = .f.
			THISFORM.Container2.Visible = .t.
		ENDIF
	ENDPROC
	
	PROCEDURE GetIdn
	LOCAL lnIdn
		lnIdn = THIS.tnId
		RETURN lnIdn	
	ENDPROC
	
	PROCEDURE SetIdn
	PARAMETERS tnIdn	
		THIS.tnId = tnIdn
	ENDPROC
	
	PROCEDURE Init()
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER("grid"),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.HighlightStyle = 2
				myobject.HighlightBackColor = oPath.tnGridColor
				myobject.HighlightForeColor = oPath.tnGridTextColor
			ENDIF
		ENDFOR
		&&
		THISFORM.Height = oPath.tnSelectHeight&&upd	
		THISFORM.Top = oPath.tnSelectTop
		THISFORM.Refresh()						
	ENDPROC
	
	PROCEDURE Resize()
	LOCAL lcForm, lcAliasname, ;
	lnPanelLeft, lnPanelTop
		IF THISFORM.WindowState = 0
			&& n
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
					myobject.Width = THISFORM.Width
					myobject.Height = THISFORM.Height - 34
				ENDIF
			ENDFOR
			
			IF THISFORM.Height != oPath.tnSelectHeight&&upd
				oPath.SetMyParam("tnSelectHeight",THISFORM.Height)
			ENDIF
			
			IF THISFORM.Top != oPath.tnSelectTop&&upd
				oPath.SetMyParam("tnSelectTop",THISFORM.Top)
			ENDIF
		ENDIF
		
		IF THISFORM.WindowState = 2
			&& max
			FOR EACH myobject IN THISFORM.Objects
				IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
					myobject.Width = THISFORM.Width + 2
					myobject.Height = THISFORM.Height - 33
				ENDIF
			ENDFOR
		ENDIF
		
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER('grid'),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.Top = 34
			ENDIF
		ENDFOR
		&&
		FOR EACH myobject IN THISFORM.Objects
			IF AT(UPPER('container'),ALLTRIM(UPPER(myobject.Name))) > 0
				myobject.Left = 0
				myobject.Top = 0
				myobject.Width = THISFORM.Width
				myobject.Height = 34
				myobject.Resize()
			ENDIF
		ENDFOR
		&& m10870
		oPath.SetCustomFont()
	ENDPROC

	PROCEDURE QueryUnload()
		LOCAL lcFormT
		lcFormT = oModel.FindForm("CustomForm")
		IF VARTYPE(lcFormT) == "O"
			lcFormT.Visible = .f.							
			lcFormT.WindowState = 2
			lcFormT.Visible = .t.
			lcFormT.Grid1.Width = THISFORM.Width
			lcFormT.Grid1.Height = THISFORM.Height
			lcFormT.Resize()
		ENDIF
		&&
		oPath.MarkMenu()
		FormDelete("selectform")			
		THISFORM.Hide()
		THISFORM.Destroy()
	ENDPROC
	
	PROCEDURE Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC

	&& Grid1
	ADD OBJECT Grid1 AS Grid WITH Name = "Grid1", ;
	AllowCellSelection = .t., BackColor = RGB(248,244,255), ColumnCount = -1, ;
	DeleteMark = .f., Height = 428, HighlightStyle = 2, Left = 0, ReadOnly = .f., ;
	RecordSourceType = 1, RowHeight = 21, ScrollBars = 3, Top = 48, View = 0, Visible = .t., Width = 640
 	
 	PROCEDURE Grid1.MyObject()
 		RETURN THIS
 	ENDPROC
 	
 	PROCEDURE Grid1.GetMouseObject()
 	LPARAMETERS tcClass	
	LOCAL loRet
	&& Получение ссылки на объект под курсором
		IF EMPTY(tcClass)
			tcClass = "Column"
		ENDIF

		LOCAL ARRAY laM[2]
		AMOUSEOBJ(laM, 1)
		IF TYPE("laM[1]") = "O" 
			IF laM[1].BaseClass = tcClass
				loRet = laM[1]
			ELSE
				IF TYPE("laM[1].ControlCount") = "N"
					FOR i = 1 TO laM[1].ControlCount
						IF laM[1].Controls(i).BaseClass = tcClass
							loRet = laM[1].Controls(i)
							EXIT 
						ENDIF 
					ENDFOR 
				ENDIF 
			ENDIF 
		ENDIF 
		&&
		RETURN loRet
 	ENDPROC
 
 	PROCEDURE Grid1.Init()
 		THIS.Refresh()
 	ENDPROC
 	
 	PROCEDURE Grid1.Click()
	LOCAL lnColIndex,lnWorkArea
		lnColIndex = THISFORM.tnColIndex
		&&
		IF lnColIndex = 1
			lnWorkArea = oModel.IsUsed("kdtable1")
			IF lnWorkArea > 0
				LOCAL lcForm
				FOR EACH myobject IN THISFORM.Controls
					IF UPPER(ALLTRIM(myobject.BaseClass)) == UPPER("grid")
						lcForm = myobject
						EXIT
					ENDIF
				ENDFOR

				IF VARTYPE(lcForm) == "O"
					SELECT (lnWorkArea)
					LOCAL lcAnswer
					IF UPPER(ALLTRIM(fl)) = UPPER("*")
						&& да->нет
						lcAnswer = ""						
					ELSE 
						IF UPPER(ALLTRIM(fl)) = UPPER("")
							&& нет->да
							lcAnswer = "*"
						ENDIF					
					ENDIF 
					&&
					REPLACE fl WITH (lcAnswer)
					lcForm.Columns(lnColIndex).Text1.Value = (lcAnswer)		
					&& изменение среди реквизитов, которые отображаем
					LOCAL lcAnswer,lnRecno
					lcAnswer = ""
					lnRecno = RECNO(lnWorkArea)
					SCAN
						lcAnswer = lcAnswer + ALLTRIM(fl) + ","
					ENDSCAN
					&&
					oPath.tcSelectAnswer = lcAnswer
					oPath.SetMyParam("tcSelectAnswer",oPath.tcSelectAnswer)
					GOTO (lnRecno) IN (lnWorkArea)			
				ENDIF
			ENDIF			
		ENDIF
 	ENDPROC
 	
 	PROCEDURE Grid1.KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lnColIndex, lnAnswer
	lnColIndex = THISFORM.tnColIndex
		lnAnswer = LASTKEY()
		SELECT ALIAS()
		THISFORM.tnRecno = RECNO(ALIAS())
		&&	
		DO CASE					
			CASE lnAnswer = 1 
				&& HOME
				IF !THISFORM.tlSearch
					GO TOP IN ALIAS()
					THIS.SetFocus()
				ENDIF

			CASE lnAnswer = 6
				&& END
				IF !THISFORM.tlSearch
					GO BOTTOM IN ALIAS()
					THIS.SetFocus()
				ENDIF
		ENDCASE	
	ENDPROC
 
 	PROCEDURE Grid1.AfterRowColChange()
 	LPARAMETERS nColIndex
		&& Номер элемента массива, который хранит дескриптор формы
		THISFORM.tnColIndex = nColIndex
 	ENDPROC
 	
 	PROCEDURE Grid1.Error()
 	LPARAMETERS nError, cMethod, nLine
		*SELECT 1
 	ENDPROC
 	 
 	&& Grid2
	ADD OBJECT Grid2 AS Grid WITH Name = "Grid2", ;
	AllowCellSelection = .t., BackColor = RGB(248,244,255), ColumnCount = -1, ;
	DeleteMark = .f., Height = 428, HighlightStyle = 2, Left = 0, ReadOnly = .t., ;
	RecordSourceType = 1, RowHeight = 21, ScrollBars = 3, Top = 48, View = 0, Visible = .f., Width = 640
 	
 	PROCEDURE Grid2.MyObject()
 		RETURN THIS
 	ENDPROC
 	
 	PROCEDURE Grid2.MyKeyPress()
 	PARAMETERS tnKeyCode
 	LOCAL lnColIndex, lnAnswer
		lnColIndex = THISFORM.tnColIndex
		lnAnswer = tnKeyCode
		SELECT ALIAS()
		&&
		DO CASE
			CASE lnAnswer = 1
				&& ctrl+a
				oCell.GridSelectAll()
				
			CASE lnAnswer = 3
				&& ctrl+c
				oCell.GridCopyOption()				
		ENDCASE
 	ENDPROC
 	
 	PROCEDURE Grid2.GetMouseObject()
 	LPARAMETERS tcClass	
	LOCAL loRet
	&& Получение ссылки на объект под курсором
		IF EMPTY(tcClass)
			tcClass = "Column"
		ENDIF

		LOCAL ARRAY laM[2]
		AMOUSEOBJ(laM, 1)
		IF TYPE("laM[1]") = "O" 
			IF laM[1].BaseClass = tcClass
				loRet = laM[1]
			ELSE
				IF TYPE("laM[1].ControlCount") = "N"
					FOR i = 1 TO laM[1].ControlCount
						IF laM[1].Controls(i).BaseClass = tcClass
							loRet = laM[1].Controls(i)
							EXIT 
						ENDIF 
					ENDFOR 
				ENDIF 
			ENDIF 
		ENDIF 
		&&
		RETURN loRet
 	ENDPROC
 
 	PROCEDURE Grid2.Init()
 		THIS.Refresh()
 	ENDPROC
 	
 	PROCEDURE Grid2.Click()
		&& Сохраняем номер выбранной строки для поиска по связанным таблицам
		THISFORM.tlSearch = .f.
		&&		
		SELECT "kdtable2"
		THISFORM.tnRecno = RECNO("kdtable2")
		GOTO (THISFORM.tnRecno) IN "kdtable2"
 	ENDPROC
 	
 	PROCEDURE Grid2.KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lnColIndex, lnAnswer
		lnColIndex = THISFORM.tnColIndex
		lnAnswer = LASTKEY()
		SELECT ALIAS()
		THISFORM.tnRecno = RECNO(ALIAS())
		&&				
		DO CASE					
			CASE lnAnswer = 1 
				&& HOME
				IF !THISFORM.tlSearch
					GO TOP IN ALIAS()
					THIS.SetFocus()
				ENDIF

			CASE lnAnswer = 6
				&& END
				IF !THISFORM.tlSearch
					GO BOTTOM IN ALIAS()
					THIS.SetFocus()
				ENDIF
		ENDCASE	
	ENDPROC
 
 	PROCEDURE Grid2.AfterRowColChange()
 	LPARAMETERS nColIndex
		&& Номер элемента массива, который хранит дескриптор формы
		THISFORM.tnColIndex = nColIndex
 	ENDPROC
 	 	
 	PROCEDURE Grid2.MouseUp()
 	LPARAMETERS nButton, nShift, nXCoord, nYCoord
 		IF nButton = 2
			oModel.DoContext()
		ENDIF
 	ENDPROC
 	
 	PROCEDURE Grid2.Error()
 	LPARAMETERS nError, cMethod, nLine
		*SELECT 1
 	ENDPROC 
 	 
 	&& Container1
 	ADD OBJECT Container1 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .t.
 	
	PROCEDURE Container1.Init()		
		&& Command1
		THIS.AddObject("Command1","CommandButton")
		WITH THIS.Command1
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\asterisk.png"
			.PictureMargin = 0
			.PicturePosition = 13
			.ToolTipText = "Выделить все/Отменить выбор (F11/F12)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command1,"Click",oSContainerEvent,"Command1Click")
		BINDEVENT(THIS.Command1,"Error",oSContainerEvent,"Command1Error")
		
		&& Command2
		THIS.AddObject("Command2","CommandButton")
		WITH THIS.Command2
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\next.png"
			.PictureMargin = 0
			.PicturePosition = 13
			.ToolTipText = "Выбрать записи по коду"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command2,"Click",oSContainerEvent,"Command2Click")
		BINDEVENT(THIS.Command2,"Error",oSContainerEvent,"Command2Error")
				
		&& Line
		LOCAL lnCnt,loTemp
		lnCnt = 1
		FOR i = 1 TO 2&&5
			THIS.AddObject("Line"+ALLTRIM(STR(lnCnt)),"Line")
			loTemp = GETPEM(THIS,"Line"+ALLTRIM(STR(lnCnt)))
			WITH loTemp
				.Height = 26
				.Width = 0
				.Visible = .t.
			ENDWITH			
			lnCnt = lnCnt + 1
		ENDFOR
	ENDPROC
	
	PROCEDURE Container1.Resize()
	LOCAL lnPanelLeft,lnPanelTop
		lnPanelTop = 4
		&&
		THIS.Command1.Left = 19 && Отменить изменения
		lnPanelLeft = THIS.Command1.Left + THIS.Command1.Width
		THIS.Command1.Top = 4
		&&
		THIS.Line1.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Line1.Left + THIS.Line1.Width
		THIS.Line1.Top = lnPanelTop
		&&
		THIS.Command2.Left = lnPanelLeft + 5 && Выбрать
		lnPanelLeft = THIS.Command2.Left + THIS.Command2.Width
		THIS.Command2.Top = lnPanelTop
	ENDPROC
	
	&& Container2
 	ADD OBJECT Container2 AS Container WITH ;
 	BackColor = RGB(219,215,210), BorderWidth = 1, Enabled = .t., SpecialEffect = 2, ;
 	Visible = .f.
 	
 	PROCEDURE Container2.AddMyItem()
		&& delete all
		FOR i = 1 TO THIS.Combo1.ListCount
			THIS.Combo1.RemoveListItem(i)
		ENDFOR
				
		&& add
		THIS.Combo1.AddItem("№ изделия") && 1
		THIS.Combo1.AddItem("Цех+№ изделия") && 2
		THIS.Combo1.AddItem("Код+№ изделия") && 3
		THIS.Combo1.AddItem("№ изд.(ст.)") && 4
		THIS.Combo1.AddItem("ДСЕ (внешний)") && 5
		THIS.Combo1.AddItem("ДСЕ (внутренний)") && 6
		THIS.Combo1.AddItem("Наименование")&& 7
		THIS.Combo1.AddItem("Признак") && 8
		THIS.Combo1.AddItem("Без индекса") && 9
		&&
		DIMENSION THISFORM.taIndexColumn(9)
		THISFORM.taIndexColumn[1] = 2
		THISFORM.taIndexColumn[2] = 3
		THISFORM.taIndexColumn[3] = 10
		THISFORM.taIndexColumn[4] = 6
		THISFORM.taIndexColumn[5] = 7
		THISFORM.taIndexColumn[6] = 1
		THISFORM.taIndexColumn[7] = 8
		THISFORM.taIndexColumn[8] = 11
		THISFORM.taIndexColumn[9] = 0
		
		THIS.Combo1.Value = 1
		THISFORM.tnCombo1 = THIS.Combo1.Value
		THIS.Combo1.DisplayCount = THIS.Combo1.ListCount
	ENDPROC

	PROCEDURE Container2.Init()
		&& Combo1
		THIS.AddObject("Combo1","ComboBox")
		WITH THIS.Combo1
			.Enabled = .t.
			.Height = 26
			.ReadOnly = .f.
			.Style = 2
			.ToolTipText = "Индекс сортировки"
			.Visible = .t.
			.Width = 175
		ENDWITH
		&&
		BINDEVENT(THIS.Combo1,"Click",oS2ContainerEvent,"Combo1Click")
		BINDEVENT(THIS.Combo1,"Error",oS2ContainerEvent,"Combo1Error")
		BINDEVENT(THIS.Combo1,"MouseMove",oS2ContainerEvent,"Combo1MouseMove")
			
		&& Text1
		THIS.AddObject("Text1","TextBox")
		WITH THIS.Text1 
			.Alignment = 0
			.BackColor = RGB(248,244,255)
			.Enabled = .t.
			.FontName = 'Consolas'
			.FontSize = 11
			.Height = 26
			.ToolTipText = "Укажите, что искать (Ctrl+F)"
			.Value = "Поиск"
			.Visible = .t.
			.Width = 202
			&&
			PUBLIC gbTextMenu1
			gbTextMenu1 = .f.
		ENDWITH
		&&
		BINDEVENT(THIS.Text1,"Click",oS2ContainerEvent,"Text1Click")
		BINDEVENT(THIS.Text1,"KeyPress",oS2ContainerEvent,"Text1KeyPress")
		BINDEVENT(THIS.Text1,"Error",oS2ContainerEvent,"Text1Error")
		BINDEVENT(THIS.Text1,"LostFocus",oS2ContainerEvent,"Text1LostFocus")
		BINDEVENT(THIS.Text1,"MouseUp",oS2ContainerEvent,"Text1MouseUp")
		
		&& Command1
		THIS.AddObject("Command1","CommandButton")
		WITH THIS.Command1
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\search.png"
			.PictureMargin = 0
			.PicturePosition = 14
			.ToolTipText = "Поиск по таблице (Enter)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command1,"Click",oS2ContainerEvent,"Command1Click")
		BINDEVENT(THIS.Command1,"Error",oS2ContainerEvent,"Command1Error")
		
		&& Command2
		THIS.AddObject("Command2","CommandButton")
		WITH THIS.Command2
			.Alignment = 2
			.BackColor = RGB(176,183,198)
			.Caption = ""
			.Enabled = .t.
			.Height = 26
			.Picture = "c:\work\men108\img\back.png"
			.PictureMargin = 0
			.PicturePosition = 13
			.ToolTipText = "Назад (F5)"
			.Visible = .t.
			.Width = 26
		ENDWITH
		&&
		BINDEVENT(THIS.Command2,"Click",oS2ContainerEvent,"Command2Click")
		BINDEVENT(THIS.Command2,"Error",oS2ContainerEvent,"Command2Error")
				
		&& Line
		LOCAL lnCnt,loTemp
		lnCnt = 1
		FOR i = 1 TO 3&&4
			THIS.AddObject("Line"+ALLTRIM(STR(lnCnt)),"Line")
			loTemp = GETPEM(THIS,"Line"+ALLTRIM(STR(lnCnt)))
			WITH loTemp
				.Height = 26
				.Width = 0
				.Visible = .t.
			ENDWITH			
			lnCnt = lnCnt + 1
		ENDFOR
		&&
		THIS.AddMyItem()
	ENDPROC
	
	PROCEDURE Container2.Resize()
	LOCAL lnPanelLeft,lnPanelTop
		lnPanelTop = 4
		&&
		THIS.Combo1.Left = 19
		lnPanelLeft = THIS.Combo1.Left + THIS.Combo1.Width
		THIS.Combo1.Top = 4
		&&
		THIS.Line1.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Line1.Left + THIS.Line1.Width
		THIS.Line1.Top = lnPanelTop
		&&
		THIS.Text1.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Text1.Left + THIS.Text1.Width
		THIS.Text1.Top = lnPanelTop
		&&
		THIS.Line2.Left = lnPanelLeft + 5
		lnPanelLeft = THIS.Line2.Left + THIS.Line2.Width
		THIS.Line2.Top = lnPanelTop
		&&
		THIS.Command1.Left = lnPanelLeft + 5 && Поиск
		lnPanelLeft = THIS.Command1.Left + THIS.Command1.Width
		THIS.Command1.Top = lnPanelTop
		&&
		THIS.Line3.Left = lnPanelLeft + 4 
		lnPanelLeft = THIS.Line3.Left + THIS.Line3.Width
		THIS.Line3.Top = lnPanelTop
		&&
		THIS.Command2.Left = lnPanelLeft + 5 && обратно к выбору kd
		lnPanelLeft = THIS.Command2.Left + THIS.Command2.Width
		THIS.Command2.Top = lnPanelTop		
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: SContainerEvent - класс для контейнера формы SContainerForm
DEFINE CLASS SContainerEvent AS Custom
	PROCEDURE Command1Click()
	LOCAL lcForm, ;
	lnWorkArea
		&& Выделить все/Отменить выбор
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			lnWorkArea = oModel.IsUsed("kdtable1")
			IF lnWorkArea > 0 &&upd
				SELECT (lnWorkArea)
				IF RECCOUNT(ALIAS()) > 0&&
					&& commandbutton = f11
					IF oPath.tbF11Press
						oPath.SetMyF11F12(.f.,.f.,"kdtable1",1)
					ELSE
						oPath.SetMyF11F12(.t.,.t.,"kdtable1",1)
					ENDIF
					
					&& изменение среди реквизитов, которые отображаем
					LOCAL lcAnswer,lnRecno&&upd
					lcAnswer = ""
					lnRecno = RECNO(lnWorkArea)
					LOCAL lnCnt
					lnCnt = 0
					&&
					SCAN					
						lcAnswer = lcAnswer + ALLTRIM(fl) + ","
					ENDSCAN

					oPath.tcSelectAnswer = lcAnswer
					oPath.SetMyParam("tcSelectAnswer",oPath.tcSelectAnswer)
					GOTO (lnRecno) IN (lnWorkArea)
				ENDIF
			ENDIF
		ENDIF		
	ENDPROC
	
	PROCEDURE Command1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
	
	PROCEDURE Command2Click()
		&& Выбрать записи по коду
		LOCAL lnWorkArea,lnWorkAreaT
		lnWorkArea = oModel.IsUsed("rtable")
		IF lnWorkArea > 0
			lnWorkAreaT = oModel.IsUsed("kdtable1")
			IF lnWorkAreaT > 0
				SELECT 0
				SELECT rtable.izm,rtable.nizd,rtable.snizd,rtable.mod,rtable.kudar,rtable.kuda,rtable.dtv,rtable.cex,rtable.rank,;
				rtable.kd,rtable.priz,rtable.naim FROM "rtable" INNER JOIN "kdtable1" ON rtable.kd = kdtable1.kd WHERE !EMPTY(kdtable1.fl) ;
				ORDER BY rtable.kd INTO CURSOR "kdtable2" READWRITE
				&&
				oModel_select.SelectData2()
			ENDIF
		ENDIF				
	ENDPROC
	
	PROCEDURE Command2Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: S2ContainerEvent - класс для контейнера формы SelectForm
DEFINE CLASS S2ContainerEvent AS Custom
	PROCEDURE Combo1Click()
	LOCAL lcForm, lcTxt
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			IF lcForm.Grid2.Visible
				SELECT "kdtable2"
				SCATTER MEMVAR
				&&
				lcTxt = TRIM(m.izm) + " " + ;				
					TRIM(m.nizd) + " " + ;
					TRIM(m.snizd) + " " + ;
					TRIM(m.mod) + " " + ;
					TRIM(m.kudar) + " " + ;
					TRIM(m.kuda) + " " + ;
					TRIM(DTOC(m.dtv)) + " " + ;
					TRIM(m.cex) + " " + ;
					TRIM(m.rank) + " " + ;
					TRIM(m.kd) + " " + ;
					TRIM(m.priz) + " " + ;
					TRIM(m.naim)
				&&
				oModel_select.SearchRecord(lcTxt, lcForm.Container2.Combo1.ListIndex) &&THIS
				lnRecno = lcForm.tnRecno
				GOTO (lnRecno) IN "kdtable2"
				lnColIndex = lcForm.tnColIndex
				lcForm.tnCombo1 = lcForm.Container2.Combo1.ListIndex
				lcForm.Grid2.Columns(lnColIndex).Text1.SetFocus()
				lcForm.Grid2.SetFocus()
			ENDIF
		ENDIF
	ENDPROC 	

	PROCEDURE Combo1MouseMove()
	LPARAMETERS nButton, nShift, nXCoord, nYCoord
		NODEFAULT
	ENDPROC
	
	PROCEDURE Combo1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC

	PROCEDURE Text1Click()
	LOCAL lcForm		
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.
			IF lcForm.tnClickSearch = 0
				lcForm.Container2.Text1.Value = ""
				lcForm.tnPressSearch = lcForm.tnPressSearch + 1
				lcForm.tnClickSearch = lcForm.tnClickSearch + 1
			ENDIF
		ENDIF
	ENDPROC

	PROCEDURE Text1KeyPress()
	LPARAMETERS nKeyCode, nShiftAltCtrl
	LOCAL lcForm	
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.
			IF lcForm.tnPressSearch = 0	
				lcForm.Container2.Text1.Value = ""
				lcForm.tnPressSearch = lcForm.tnPressSearch + 1
				lcForm.tnClickSearch = lcForm.tnClickSearch + 1
			ENDIF
		ENDIF
	ENDPROC

	PROCEDURE Text1MyKeyPress()
	PARAMETERS tnKeyCode
	LOCAL lcForm,lcTemp,lcSymbol, ;
	lnSelStart,lnSelLength, ;
	loNode
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			loNode = oModel.MyObject()
			lnSelStart = loNode.SelStart
			lnSelLength = loNode.SelLength
			&&
			DO CASE 
				CASE tnKeyCode = 1
					&& ctrl+a
					oCell.TextSelectAll()
					
				CASE tnKeyCode = 3
					&& ctrl+c
					gbTextMenu1 = .t.
					oCell.TextCopyOption()
					lcForm.Container2.Text1.SetFocus()								
			ENDCASE			
		ENDIF
	ENDPROC

	PROCEDURE Text1LostFocus()
	LOCAL lcForm
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"	
			IF TYPE("gbTextMenu1") != "U"
				IF gbTextMenu1
					&& чтобы фокус не уходил из text1
					gbTextMenu1 = .f.
					NODEFAULT			
					lcForm.Container2.Text1.SetFocus()
				ENDIF
			ENDIF
		ENDIF	
	ENDPROC

	PROCEDURE Text1MouseUp()
 	LPARAMETERS nButton, nShift, nXCoord, nYCoord
 	LOCAL lcForm
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.tlSearch = .t.			
			IF nButton = 2
	 			IF lcForm.tnClickSearch = 0
					lcForm.Container2.Text1.Value = ""
					lcForm.tnPressSearch = lcForm.tnPressSearch + 1
					lcForm.tnClickSearch = lcForm.tnClickSearch + 1
				ENDIF
				oModel.DoText()
			ENDIF
		ENDIF	
 	ENDPROC
	
	PROCEDURE Text1LostFocus()
	LOCAL lcForm
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"	
			IF TYPE("gbTextMenu1") != "U"
				IF gbTextMenu1
					&& чтобы фокус не уходил из text1
					gbTextMenu1 = .f.
					NODEFAULT			
					lcForm.Container2.Text1.SetFocus()
				ENDIF
			ENDIF
		ENDIF	
	ENDPROC
	
	PROCEDURE Text1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC

	PROCEDURE Command1Click()
	LOCAL lcForm, lcValue, lcChoiceSearch
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"		
			lcValue = ALLTRIM(UPPER(lcForm.Container2.Text1.Value))
			IF lcForm.Grid2.Visible
				IF !EMPTY(lcValue)		
					DO CASE 			
						CASE lcForm.Container2.Combo1.ListIndex = 1		
							&& nizd
							oModel_select.Search(2,lcValue)

						CASE lcForm.Container2.Combo1.ListIndex = 2
							&& cn
							oModel_select.Search(3,lcValue)
						
						CASE lcForm.Container2.Combo1.ListIndex = 3
							&& kdn
							oModel_select.Search(10,lcValue)
							
						CASE lcForm.Container2.Combo1.ListIndex = 4
							&& snizd
							oModel_select.Search(6,lcValue)
							
						CASE lcForm.Container2.Combo1.ListIndex = 5
							&& kdr
							oModel_select.Search(7,lcValue)
							
						CASE lcForm.Container2.Combo1.ListIndex = 6
							&& kd
							oModel_select.Search(1,lcValue)
							
						CASE lcForm.Container2.Combo1.ListIndex = 7
							&& naim
							oModel_select.Search(8,lcValue)
							
						CASE lcForm.Container2.Combo1.ListIndex = 8
							&& priz
							oModel_select.Search(11,lcValue)
							
						CASE lcForm.Container2.Combo1.ListIndex = 9
							&& no
							oModel_select.Search(0,lcValue)
					ENDCASE
				ENDIF
				&&
				lcForm.Grid2.Refresh()
			ENDIF			
		ENDIF
	ENDPROC

	PROCEDURE Command1Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC

	PROCEDURE Command2Click()
	LOCAL lcForm
		lcForm = oModel.FindForm("SelectForm")
		IF VARTYPE(lcForm) == "O"
			lcForm.FormSettings(1)
		ENDIF
	ENDPROC

	PROCEDURE Command2Error()
	LPARAMETERS nError, cMethod, nLine
		SELECT 1
	ENDPROC
ENDDEFINE 
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& Class: HelpForm - помощь
*!*	DEFINE CLASS HelpForm AS Form
*!*		BackColor = RGB(219,215,210)
*!*		BorderStyle = 3
*!*		Caption = ""
*!*		Closable = .t.
*!*		Height = 200
*!*		KeyPreview = .t.
*!*		Left = 23
*!*		MaxButton = .t.
*!*		ScrollBars = 0
*!*		ShowInTaskBar = .t.
*!*		ShowTips = .t.
*!*		ShowWindow = 0
*!*		Top = 23
*!*		Visible = .t.
*!*		Width = 200
*!*		WindowState = 0
*!*		WindowType = 0

*!*		DIMENSION taControls(1)
*!*		tcPre = "Помощь"
*!*		tnImageHeight = 0
*!*		tnImageWidth = 0
*!*		tnHeight = 0
*!*		tnWidth = 0
*!*		
*!*		PROCEDURE taControlsInit()
*!*		LOCAL lnCnt
*!*			lnCnt = 0
*!*			FOR EACH mycontrol IN THISFORM.Controls
*!*				lnCnt = lnCnt + 1
*!*				DIMENSION THISFORM.taControls(lnCnt,5)
*!*				THISFORM.taControls[lnCnt,1] = mycontrol.Name
*!*				THISFORM.taControls[lnCnt,2] = mycontrol.Height
*!*				THISFORM.taControls[lnCnt,3] = mycontrol.Width
*!*				THISFORM.taControls[lnCnt,4] = mycontrol.Top
*!*				THISFORM.taControls[lnCnt,5] = mycontrol.Left
*!*			ENDFOR
*!*		ENDPROC
*!*		
*!*		PROCEDURE taControlsGet()
*!*		PARAMETERS tcName,tcProperty
*!*			&& передаем имя элемента формы, получаем его номер в массиве
*!*			FOR i = 1 TO ALEN(THISFORM.taControls,1)
*!*				IF UPPER(ALLTRIM(THISFORM.taControls[i,1])) == UPPER(ALLTRIM(tcName))
*!*					IF VARTYPE(tcProperty) == "C"
*!*						DO CASE 						
*!*							CASE ALLTRIM(UPPER(tcProperty)) == ALLTRIM(UPPER("height"))
*!*								RETURN THISFORM.taControls[i,2]					
*!*								
*!*							CASE ALLTRIM(UPPER(tcProperty)) == ALLTRIM(UPPER("width"))
*!*								RETURN THISFORM.taControls[i,3]					
*!*								
*!*							CASE ALLTRIM(UPPER(tcProperty)) == ALLTRIM(UPPER("top"))
*!*								RETURN THISFORM.taControls[i,4]					
*!*								
*!*							CASE ALLTRIM(UPPER(tcProperty)) == ALLTRIM(UPPER("left"))
*!*								RETURN THISFORM.taControls[i,5]
*!*						ENDCASE
*!*					ELSE			
*!*						RETURN i
*!*					ENDIF				
*!*				ENDIF
*!*			ENDFOR
*!*			RETURN 0
*!*		ENDPROC
*!*		
*!*		PROCEDURE MyImageWidth()
*!*		PUBLIC gaImage(5,2)
*!*			gaImage[1,1] = "help_1.png"
*!*			gaImage[2,1] = "help_2.png"
*!*			gaImage[3,1] = "help_3.png"
*!*			gaImage[4,1] = "help_4.png"
*!*			gaImage[5,1] = "help_5.png"
*!*			&&
*!*			gaImage[1,2] = "Укажите таблицу '.dbf'. По умолчанию, уже указан путь к нужному файлу. Не меняйте его за исключением случаев, когда это необходимо." + CHR(13) + ;
*!*			"Клавиши быстрого доступа: " + CHR(13) + CHR(10) + ;
*!*			"F1 - Открыть подсказку (работает во всей программе)," + CHR(13) + CHR(10) + ;
*!*			"ENTER - Открыть таблицу," + CHR(13) + CHR(10) + ;
*!*			"ESC - Выход из программы." + CHR(13) + CHR(10)
*!*			&&
*!*			gaImage[2,2] = "Окно 'Перечень изделий'. Выберите нужные изделия (символ '*' в 1-м столбце) и нажмите кнопку 'Выбор' (->)." + CHR(13) + CHR(10) + ;
*!*			"Клавиши быстрого доступа: " + CHR(13) + CHR(10) + ;
*!*			"F11 - Выбрать все строки," + CHR(13) + CHR(10) + ;
*!*			"F12 - Отменить выбор и убрать 'звездочки' (символ *)," + CHR(13) + CHR(10) + ;
*!*			"Ctrl+W - Перейти к просмотру материалов по выбранным изделиям." + CHR(13) + CHR(10)
*!*			&&
*!*			gaImage[3,2] = "Окно 'Материалы'. Используйте сортировки для поиска по нужным реквизитам. Вы можете отметить нужные строки нажатием на ячейку в 1-м столбце и сохранить в файл '.dbf'." + CHR(13) + CHR(10) + ;
*!*			"Клавиши быстрого доступа: " + CHR(13) + CHR(10) + ;
*!*			"F11 - Выбрать все строки," + CHR(13) + CHR(10) + ;
*!*			"F12 - Отменить выбор и убрать 'звездочки' (символ '*')," + CHR(13) + CHR(10) + ;
*!*			"F1 - Открыть подсказку (работает во всей программе)," + CHR(13) + CHR(10) + ;
*!*			"F2 - Сортировка по номеру изделия, " + CHR(13) + CHR(10) + ;
*!*			"F3 - Сортировка по коду материала, " + CHR(13) + CHR(10) + ;
*!*			"F4 - Сортировка по наименованию, " + CHR(13) + CHR(10) + ;
*!*			"F5 - Возврат к перечню изделий, " + CHR(13) + CHR(10) + ;
*!*			"F6 - Сохранить в файл. " + CHR(13) + CHR(10) + ;
*!*			"Ctrl+F - Перейти в окно поиска, " + CHR(13) + CHR(10) + ;
*!*			"ESC - Выход из программы."
*!*			&&
*!*			gaImage[4,2] = "Окно 'Сохранить файл'. В этом окне вы можете посмотреть файл, в который хотите сохранить строки (файл будет дозаписан), или создать новый, очистить существующий." + CHR(13) + CHR(10) + ;
*!*			"F1 - Открыть подсказку (работает во всей программе)," + CHR(13) + CHR(10) + ;
*!*			"F5 - Закрыть окно, " + CHR(13) + CHR(10) + ;
*!*			"ESC - Выход из программы."
*!*			&&
*!*			gaImage[5,2] = "Окно 'Выбранные строки'. Доступны поиски/сортировки и удаление строк." + CHR(13) + CHR(10) + ;
*!*			"Клавиши быстрого доступа: " + CHR(13) + CHR(10) + ;
*!*			"F11 - Выбрать все строки," + CHR(13) + CHR(10) + ;
*!*			"F12 - Отменить выбор и убрать 'звездочки' (символ '*')," + CHR(13) + CHR(10) + ;
*!*			"F1 - Открыть подсказку (работает во всей программе)," + CHR(13) + CHR(10) + ;
*!*			"F2 - Сортировка по номеру изделия, " + CHR(13) + CHR(10) + ;
*!*			"F3 - Сортировка по коду материала, " + CHR(13) + CHR(10) + ;
*!*			"F4 - Сортировка по наименованию, " + CHR(13) + CHR(10) + ;
*!*			"F5 - Возврат к перечню изделий, " + CHR(13) + CHR(10) + ;
*!*			"F6 - Сохранить в файл. " + CHR(13) + CHR(10) + ;
*!*			"Ctrl+F - Перейти в окно поиска, " + CHR(13) + CHR(10) + ;
*!*			"Ctrl+X - Удалить строку/выбранные строки, " + CHR(13) + CHR(10) + ;
*!*			"ESC - Выход из программы."
*!*		ENDPROC
*!*			
*!*		PROCEDURE Init()
*!*			THIS.MyImageWidth()
*!*			THIS.Edit1.Value = gaImage[1,2]
*!*			&& Line
*!*			LOCAL loTemp,lnHeight,lnWidth
*!*			&&
*!*			FOR i = 1 TO 5
*!*				THIS.AddObject("Line"+ALLTRIM(STR(i)),"Line")
*!*				loTemp = GETPEM(THIS,"Line"+ALLTRIM(STR(i)))
*!*				&&
*!*				IF i >= 1 AND i <= 3
*!*					lnHeight = 0
*!*					lnWidth = 1
*!*				ENDIF 
*!*				
*!*				IF i > 3 AND i <= 5
*!*					lnHeight = 1
*!*					lnWidth = 0
*!*				ENDIF
*!*				&&
*!*				WITH loTemp
*!*					.Enabled = .t.
*!*					.Visible = .t.
*!*					.Height = lnHeight				
*!*					.Width = lnWidth										
*!*				ENDWITH
*!*			ENDFOR		
*!*			&&
*!*			THISFORM.Refresh()
*!*		ENDPROC
*!*		
*!*		PROCEDURE KeyPress()
*!*		LPARAMETERS nKeyCode, nShiftAltCtrl
*!*			IF LASTKEY() = 19 
*!*				&& -> 1
*!*				THISFORM.Label1.Click()
*!*				RETURN 
*!*			ENDIF
*!*						
*!*			IF LASTKEY() = 4
*!*				&& <- 2
*!*				THISFORM.Label2.Click()
*!*				RETURN
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE Resize()
*!*		LOCAL lnHBefore,lnWBefore, ;
*!*			lnHNow,lnWNow
*!*			&&
*!*			lnHBefore = THISFORM.tnHeight
*!*			lnWBefore = THISFORM.tnWidth
*!*			&& lnHNow,lnWNow > 0 = форма увеличилась,< 0 = уменьшилась. Значение изменения параметров по сравнению с сохраненными
*!*			lnHNow = THISFORM.Height - lnHBefore
*!*			lnWNow = THISFORM.Width - lnWBefore
*!*			LOCAL lnCnt
*!*			lnCnt = THISFORM.taControlsGet("image1")
*!*			IF lnCnt > 0
*!*				&& исходные размеры Image1
*!*				&& top
*!*				THISFORM.Label2.Left = THISFORM.Width - THISFORM.Label2.Width - 1	
*!*				THISFORM.Line1.Width = THISFORM.Width - 1
*!*				&& center
*!*				THISFORM.Image1.Height = THISFORM.taControlsGet("image1","height")
*!*				THISFORM.Image1.Left = lnWNow * 0.5
*!*				&& bottom
*!*				THISFORM.Edit1.Height = THISFORM.Height - THISFORM.Edit1.Top
*!*				THISFORM.Edit1.Width = THISFORM.Width
*!*				&&
*!*				THISFORM.Line3.Top = THISFORM.Image1.Top + THISFORM.Image1.Height
*!*				THISFORM.Line3.Width = THISFORM.Width - 2
*!*				THISFORM.Line4.Height = THISFORM.Image1.Height + 1
*!*				THISFORM.Line4.Left = THISFORM.Edit1.Left
*!*				THISFORM.Line4.Top = THISFORM.Image1.Top - 1						
*!*				THISFORM.Line5.Height = THISFORM.Image1.Height + 1
*!*				THISFORM.Line5.Left = THISFORM.Edit1.Left + THISFORM.Edit1.Width
*!*				THISFORM.Line5.Top = THISFORM.Image1.Top - 1
*!*			ENDIF
*!*			&&										
*!*			oPath.SetCustom()
*!*		ENDPROC
*!*		
*!*		PROCEDURE QueryUnload()
*!*			FormDelete("helpform")
*!*			THISFORM.Hide()
*!*			THISFORM.Destroy()
*!*		ENDPROC
*!*		
*!*		PROCEDURE Error()
*!*		LPARAMETERS nError, cMethod, nLine
*!*			SELECT 1
*!*		ENDPROC
*!*		
*!*		ADD OBJECT Label1 AS Label WITH Alignment = 0, AutoSize = .t., BackColor = RGB(219,215,210), ;
*!*		Caption = '< Назад', Enabled = .t., FontName = 'Consolas', FontSize = 11, Height = 26, Left = 10, ;
*!*		Top = 0, Visible = .t., Width = 175
*!*		
*!*		PROCEDURE Label1.Click()
*!*		LOCAL lcForm, lcFormT
*!*			&& назад
*!*			lcForm = oModel.FindForm("HelpForm")
*!*			IF VARTYPE(lcForm) == "O"
*!*				FOR i = 1 TO ALEN(gaImage,1)			
*!*					LOCAL lnFind, lcValue
*!*					lnFind = RAT("\",THISFORM.Image1.Picture)
*!*					IF lnFind > 0
*!*						lcValue = SUBSTR(THISFORM.Image1.Picture,lnFind+1)
*!*						IF ALLTRIM(UPPER(lcValue)) = ALLTRIM(UPPER(gaImage[i,1]))
*!*							IF i = 1
*!*								THISFORM.Image1.Picture = gaImage[ALEN(gaImage,1),1]
*!*								THISFORM.Edit1.Value = gaImage[ALEN(gaImage,1),2]
*!*							ELSE
*!*								THISFORM.Image1.Picture = gaImage[i-1,1]
*!*								THISFORM.Edit1.Value = gaImage[i-1,2]
*!*							ENDIF
*!*							&&
*!*							THISFORM.Refresh()						
*!*							EXIT							
*!*						ENDIF 
*!*					ENDIF						
*!*				ENDFOR
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE Label1.Error()
*!*		LPARAMETERS nError, cMethod, nLine
*!*			SELECT 1
*!*		ENDPROC
*!*		
*!*		ADD OBJECT Label2 AS Label WITH Alignment = 0, AutoSize = .t., BackColor = RGB(219,215,210), ;
*!*		Caption = 'Вперед >', Enabled = .t., FontName = 'Consolas', FontSize = 11, Height = 26, Left = 10, ;
*!*		Top = 0, Visible = .t., Width = 175
*!*		
*!*		PROCEDURE Label2.Click()
*!*		LOCAL lcForm, lcFormT
*!*			&& вперед
*!*			lcForm = oModel.FindForm("HelpForm")
*!*			IF VARTYPE(lcForm) == "O"
*!*				FOR i = 1 TO ALEN(gaImage,1)			
*!*					LOCAL lnFind, lcValue
*!*					lnFind = RAT("\",THISFORM.Image1.Picture)
*!*					IF lnFind > 0
*!*						lcValue = SUBSTR(THISFORM.Image1.Picture,lnFind+1)
*!*						IF ALLTRIM(UPPER(lcValue)) = ALLTRIM(UPPER(gaImage[i,1]))
*!*							IF i = ALEN(gaImage,1)
*!*								THISFORM.Image1.Picture = gaImage[1,1]
*!*								THISFORM.Edit1.Value = gaImage[1,2]
*!*							ELSE
*!*								THISFORM.Image1.Picture = gaImage[i+1,1]
*!*								THISFORM.Edit1.Value = gaImage[i+1,2]
*!*							ENDIF
*!*							&&
*!*							THISFORM.Refresh()
*!*							THISFORM.Edit1.Refresh()					
*!*							EXIT							
*!*						ENDIF 
*!*					ENDIF						
*!*				ENDFOR
*!*			ENDIF
*!*		ENDPROC
*!*		
*!*		PROCEDURE Label2.Error()
*!*		LPARAMETERS nError, cMethod, nLine
*!*			SELECT 1
*!*		ENDPROC
*!*		
*!*		ADD OBJECT Image1 AS Image WITH Alignment = 2, Enabled = .t., Picture = "help_1.png", Visible = .t.
*!*		
*!*		PROCEDURE Image1.Init()
*!*			THIS.BorderStyle = 1
*!*			THIS.Stretch = 0
*!*		ENDPROC
*!*		
*!*		ADD OBJECT Edit1 AS EditBox WITH Alignment = 0, BackColor = RGB(219,215,210), ;
*!*		Enabled = .t., FontName = 'Consolas', FontSize = 11, Height = 26, IntergralHeight = .t., Left = 10, ScrollBars = 2, ;
*!*		Stretch = 2, ReadOnly = .t., Top = 0, Visible = .t., Width = 175
*!*	ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: PathwayForm - основной класс
DEFINE CLASS PathwayForm AS CUSTOM
	DIMENSION taForms(1)
	tcFontName = "Consolas"
	tnFontSize = 11
	tbFontBold = .f.
	tbFontItalic = .f.	
	tcPathExe = ""
	tcVersion = "v2.0"
	tcVersionName = "men108"
	tcVersionEngine = "v3" &&m22101_upd/m21303_upd = v0.1, men10148(zz048) = v1, m405421/m407423/men108 = v2, m21303 = v3
	tcDate = ""
	tcPathFile = ""
	tcAlias1 = "rtable"	
	tcRequisites = "Пр.изм.,№ изделия,№ изд.(ст.),Мод,Код изд.(внешний),Код изд.(внутренний),Дата ввода,Цех,Ранг,Код,Пр.,Наименование"
	tcRequisitesAnswer = ""
	tcPriz = ""
	tcPrizAnswer = ""
	tcSelect = ""
	tcSelectAnswer = ""
	tnForm = 1 && выбранная форма
	tnScreenHeight = 512 &&upd
	tnSelectHeight = 336 &&upd
	tnSelectTop = 544
	tnGridColor = 8421376
	tnGridTextColor = 16777215
	tnScreenBackColor = 16777215
	tnReqFormWidth = 264&&upd
	tnReqFormHeight = 410
	tnPrizFormWidth = 187
	tnPrizFormHeight = 319
	tnCheckKdFormWidth = 273
	tnCheckKdFormHeight = 35
	tbF11Press = .f.
	tbF12Press = .f.
					
	PROCEDURE ScreenStartForm()
		WITH _SCREEN
			.AlwaysOnTop = .f.
			.BorderStyle = 1
			.MaxButton = .t.
			.Caption = THIS.GetVersion()
			.Visible = .t.
			.WindowState = 2
		ENDWITH
	ENDPROC
	
	PROCEDURE Init()
		PUBLIC oModel,oColumnHeader,oMContainerEvent,oFContainerEvent,oCContainerEvent,oRContainerEvent, ;
		oIContainerEvent,oPContainerEvent,oCKContainerEvent,oSContainerEvent,oS2ContainerEvent
		THIS.SetPath()
		oModel = CREATEOBJECT("Model")
		oColumnHeader = CREATEOBJECT("ColumnHeader")
		oMContainerEvent = CREATEOBJECT("MContainerEvent")
		oFContainerEvent = CREATEOBJECT("FContainerEvent")
		oCContainerEvent = CREATEOBJECT("CContainerEvent")
		oRContainerEvent = CREATEOBJECT("RContainerEvent")
		oIContainerEvent = CREATEOBJECT("IContainerEvent")
		oPContainerEvent = CREATEOBJECT("PContainerEvent")
		oCKContainerEvent = CREATEOBJECT("CKContainerEvent")
		oSContainerEvent = CREATEOBJECT("SContainerEvent")
		oS2ContainerEvent = CREATEOBJECT("S2ContainerEvent")
		THIS.VersionNow()
		THIS.ScreenStartForm()
		&&
		THIS.CreateMyFoxUser()
		THIS.SetCustom()
	ENDPROC
	
	PROCEDURE SetMyF11F12()&&upd
	PARAMETERS tbValue1,tbValue2,tcAlias,tnMode
	LOCAL lcForm,lcAlias,;
	lcTrue,lcFalse,lcField, ;
	lnRecno,lnColIndex
		IF tnMode = 1
			lcTrue = "*"
			lcFalse = ""
			lnColIndex = 1
			lcField = "fl"
		ENDIF
		
		IF tnMode = 2
			lcTrue = "Да"
			lcFalse = "Нет"
			lnColIndex = 2
			lcField = "value"
		ENDIF		
		&&
		lcForm = _SCREEN.ActiveForm
		lcAlias = _SCREEN.ActiveForm.Name
		&&
		THIS.tbF11Press = tbValue1
		THIS.tbF12Press = tbValue2
		&&
		SELECT (tcAlias)
		lnRecno = RECNO(tcAlias)
		&&
		IF RECCOUNT(ALIAS()) > 0
			FOR EACH myobject IN lcForm.Controls
				IF ALLTRIM(UPPER(myobject.Class)) == ALLTRIM(UPPER("grid"))
					IF ALLTRIM(UPPER(myobject.RecordSource)) == ALLTRIM(UPPER(tcAlias))
						IF tbValue1 AND !tbValue2 OR tbValue1 AND tbValue2
							&& empty->full
							IF myobject.ColumnCount > 0							
								*UPDATE (tcAlias) SET @myrecordsource.value = (lcTrue)&&"Да"
								REPLACE(lcField) WITH (lcTrue) ALL
								GOTO (lnRecno) IN (tcAlias)
								myobject.Columns(lnColIndex).Text1.Value = (lcTrue)&&"Да"
							ENDIF
						ENDIF
						
						IF !tbValue1 AND tbValue2 OR !tbValue1 AND !tbValue2
							&& full->empty
							IF myobject.ColumnCount > 0
								*UPDATE (tcAlias) SET myrecordsource.value = (lcFalse)&&"Нет"
								REPLACE (lcField) WITH (lcFalse) ALL
								GOTO (lnRecno) IN (tcAlias)						
								myobject.Columns(lnColIndex).Text1.Value = (lcFalse)&&"Нет"
							ENDIF
						ENDIF
						RETURN .t.
					ENDIF
				ENDIF
			ENDFOR
		ENDIF
	ENDPROC
	
	PROCEDURE VersionNow()
	LOCAL lcVersion, lcDate
		&& Установить текущую версию
		lcVersion = ALLTRIM(UPPER(THIS.tcVersionName)) + " | " + THIS.GetVersion()
		lcDate = "08.12.2020"
		THIS.SetVersion(lcVersion)
		THIS.SetDate(lcDate)
	ENDPROC
	
	PROCEDURE GetVersion()
		RETURN THIS.tcVersion	
	ENDPROC
	
	PROCEDURE SetVersion()
	PARAMETERS tcVersion	
		THIS.tcVersion = tcVersion			
	ENDPROC
	
	PROCEDURE GetDate()
		RETURN THIS.tcDate		
	ENDPROC
	
	PROCEDURE SetDate()
	PARAMETERS tcDate	
		THIS.tcDate = tcDate	
	ENDPROC
	
	PROCEDURE SetPath()
		THIS.tcPathExe = SYS(5)+ SYS(2003)
	ENDPROC 
	
	PROCEDURE GetPath()
		RETURN THIS.tcPathExe
	ENDPROC
	
	PROCEDURE CreateMyFoxUser()
	LOCAL lcValue,lnRecno
		SET DEFAULT TO C:\FOXPRO2\
		DIMENSION laArray(1)
		AMEMBERS(laArray,THIS.Class,1,"U")
		&&
		IF FILE("customfoxuser.dbf")
			&& открыть
			SELECT 0
			USE "customfoxuser.dbf" ALIAS "myfoxuser" SHARED&&вопрос -> проверка, не занята ли таблица
			SET ORDER TO np
			&& значения есть -> загружаем в качестве значений по умолчанию
			IF USED("myfoxuser")						
				FOR i = 1 TO ALEN(laArray,1)
					IF ALLTRIM(UPPER(laArray[i,2])) == ALLTRIM(UPPER("property"))
						lcValue = GETPEM(THIS.Class,laArray[i,1])
						IF THIS.FindParam(laArray[i,1])
							&& найдено						
							DO CASE
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tcFontName")
									THIS.tcFontName = myfoxuser.prgvalue
								
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tnFontSize")
									THIS.tnFontSize = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tbFontBold")
									THIS.tbFontBold = CAST(myfoxuser.prgvalue AS Logical)
								
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tbFontItalic")
									THIS.tbFontItalic = CAST(myfoxuser.prgvalue AS Logical)
							
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tnGridColor")
									THIS.tnGridColor = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tnGridTextColor")
									THIS.tnGridTextColor = VAL(myfoxuser.prgvalue)
								
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tnScreenBackColor")
									THIS.tnScreenBackColor = VAL(myfoxuser.prgvalue)
								
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tnReqFormWidth")&&upd
									THIS.tnReqFormWidth = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tnReqFormHeight")
									THIS.tnReqFormHeight = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tcRequisites")
									THIS.tcRequisites = myfoxuser.prgvalue
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tcRequisitesAnswer")
									THIS.tcRequisitesAnswer = myfoxuser.prgvalue
									
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tnPrizFormWidth")&&upd
									THIS.tnPrizFormWidth = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) = UPPER("tnPrizFormHeight")
									THIS.tnPrizFormHeight = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tcPriz")
									THIS.tcPriz = myfoxuser.prgvalue
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tcPrizAnswer")
									THIS.tcPrizAnswer = myfoxuser.prgvalue
																	
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tnCheckKdFormWidth")
									THIS.tnCheckKdFormWidth = myfoxuser.prgvalue
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tnCheckKdFormHeight")
									THIS.tnCheckKdFormHeight = myfoxuser.prgvalue
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tnScreenHeight")
									THIS.tnScreenHeight = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tnSelectHeight")
									THIS.tnSelectHeight = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tnSelectTop")
									THIS.tnSelectTop = VAL(myfoxuser.prgvalue)
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tcSelect")
									THIS.tcSelect = myfoxuser.prgvalue
									
								CASE ALLTRIM(UPPER(laArray[i,1])) == UPPER("tcSelectAnswer")
									THIS.tcSelectAnswer = myfoxuser.prgvalue
							ENDCASE
						ELSE
							&& по какой-то причине параметра нет -> создать
							THIS.SetMyParam(laArray[i,1],lcValue)
						ENDIF
					ENDIF
				ENDFOR
			ENDIF
		ELSE
			&& создать
			CREATE TABLE "customfoxuser.dbf" CODEPAGE = 866 (prgname c(254), prgparam c(254), prgvalue c(254))
			INDEX ON SUBSTR(prgname,1,120)+SUBSTR(prgparam,1,120) TAG np
			REINDEX
			USE
			&&
			SELECT 0
			USE "customfoxuser.dbf" ALIAS "myfoxuser" SHARED&&вопрос -> проверка, не занята ли таблица
			SET ORDER TO np
			&& сохраненных значений нет -> не проводим загрузку параметров
			IF USED("myfoxuser")						
				FOR i = 1 TO ALEN(laArray,1)
					IF ALLTRIM(UPPER(laArray[i,2])) == ALLTRIM(UPPER("property"))
						lcValue = GETPEM(THIS.Class,laArray[i,1])
						THIS.SetMyParam(laArray[i,1],lcValue)
					ENDIF
				ENDFOR	
			ENDIF
		ENDIF
	ENDPROC
	
	PROCEDURE SetMyParam()
	PARAMETERS tcParam,tcValue
	LOCAL lbReturn
		lbReturn = .f.
		IF !THIS.FindParam(tcParam)
			&& параметр не найден -> создать
			DO CASE
				CASE VARTYPE(tcValue) == "L"								
					INSERT INTO "customfoxuser.dbf" (prgname,prgparam,prgvalue) VALUES (THIS.tcVersionName,tcParam,CAST(tcValue AS Char))
					
				CASE VARTYPE(tcValue) == "C"
					INSERT INTO "customfoxuser.dbf" (prgname,prgparam,prgvalue) VALUES (THIS.tcVersionName,tcParam,ALLTRIM(tcValue))

				CASE VARTYPE(tcValue) == "N"
					INSERT INTO "customfoxuser.dbf" (prgname,prgparam,prgvalue) VALUES (THIS.tcVersionName,tcParam,ALLTRIM(STR(tcValue)))
			ENDCASE
			lbReturn = .t.
		ELSE	
			&& параметр найден -> обновить			
			lnRecno = RECNO("myfoxuser")
			DO CASE
				CASE VARTYPE(tcValue) == "L"
					IF ALLTRIM(UPPER(myfoxuser.prgvalue)) != ALLTRIM(UPPER(CAST(tcValue AS Char)))	
						UPDATE "customfoxuser.dbf" SET prgvalue = CAST(tcValue AS Char) WHERE ALLTRIM(UPPER(prgparam)) == ALLTRIM(UPPER(tcParam)) AND RECNO() = lnRecno
					ENDIF
					
				CASE VARTYPE(tcValue) == "C"
					IF ALLTRIM(UPPER(myfoxuser.prgvalue)) != ALLTRIM(UPPER(tcValue))	
						UPDATE "customfoxuser.dbf" SET prgvalue = ALLTRIM(tcValue) WHERE ALLTRIM(UPPER(prgparam)) == ALLTRIM(UPPER(tcParam)) AND RECNO() = lnRecno
					ENDIF

				CASE VARTYPE(tcValue) == "N"
					IF ALLTRIM(UPPER(myfoxuser.prgvalue)) != ALLTRIM(UPPER(STR(tcValue)))
						UPDATE "customfoxuser.dbf" SET prgvalue = ALLTRIM(STR(tcValue)) WHERE ALLTRIM(UPPER(prgparam)) == ALLTRIM(UPPER(tcParam)) AND RECNO() = lnRecno
					ENDIF
			ENDCASE
			lbReturn = .t.
		ENDIF												
		RETURN lbReturn
	ENDPROC
	
	PROCEDURE FindParam()
	PARAMETERS tcName
	LOCAL lcTabname, ;
		lnTagCnt, ;
		lbFind
		&& Проверка наличия параметра
		lbFind = .f.	
		IF USED("myfoxuser")
			SELECT "myfoxuser"
			SCAN
				IF ALLTRIM(UPPER(myfoxuser.prgname)) == ALLTRIM(UPPER(THIS.tcVersionName)) AND ;
				ALLTRIM(UPPER(myfoxuser.prgparam)) == ALLTRIM(UPPER(tcName))
					lbFind = .t.
					EXIT
				ENDIF
			ENDSCAN
		ENDIF
		RETURN lbFind	
	ENDPROC
	
	PROCEDURE SetCustomFont&&upd container
	LOCAL lcForm,loMyObject
		&& Шрифт
		FOR EACH lcForm IN Application.Objects 
			FOR EACH loNode IN lcForm.Objects
				THIS.MyCustomFont(loNode)
				
				IF ALLTRIM(UPPER(loNode.Class)) == UPPER("container")
					FOR EACH myobject IN loNode.Objects
						THIS.MyCustomFont(myobject)
					ENDFOR
				ENDIF 
			ENDFOR				
		ENDFOR				
	ENDPROC
	
	PROCEDURE MyCustomFont()&&upd
	PARAMETERS toNode
		IF ALLTRIM(UPPER(toNode.Class)) == UPPER("checkbox") OR ;
			ALLTRIM(UPPER(toNode.Class)) == UPPER("combobox") OR ;
			ALLTRIM(UPPER(toNode.Class)) == UPPER("commandbutton") OR ;
			ALLTRIM(UPPER(toNode.Class)) == UPPER("editbox") OR ;
			ALLTRIM(UPPER(toNode.Class)) == UPPER("grid") OR ;
			ALLTRIM(UPPER(toNode.Class)) == UPPER("label") OR ;
			ALLTRIM(UPPER(toNode.Class)) == UPPER("textbox")
			&&
			toNode.FontName = ALLTRIM(UPPER(THIS.tcFontName))
			toNode.FontSize = THIS.tnFontSize
			toNode.FontBold = THIS.tbFontBold
			toNode.FontItalic = THIS.tbFontItalic
			toNode.Refresh()
		ENDIF
	ENDPROC
	
	PROCEDURE SetCustomGridColor
		FOR EACH loNode IN Application.Objects 
			FOR EACH myobject IN loNode.Objects
				IF ALLTRIM(UPPER(myobject.Class)) == UPPER("grid")
					myobject.HighlightBackColor = THIS.tnGridColor
				ENDIF
			ENDFOR				
		ENDFOR
	ENDPROC
	
	PROCEDURE SetCustomGridTextColor
		FOR EACH loNode IN Application.Objects 
			FOR EACH myobject IN loNode.Objects
				IF ALLTRIM(UPPER(myobject.Class)) == UPPER("grid")
					myobject.HighlightForeColor = THIS.tnGridTextColor
				ENDIF								 
			ENDFOR	
		ENDFOR
	ENDPROC
	
	PROCEDURE SetCustomScreenBackColor
		&& Цвет фона главного окна
		_SCREEN.BackColor = THIS.tnScreenBackColor
	ENDPROC
	
	PROCEDURE SetCustom()
		THIS.SetCustomFont()
		THIS.SetCustomGridColor()
		THIS.SetCustomGridTextColor()
		THIS.SetCustomScreenBackColor()
	ENDPROC
	
	PROCEDURE MarkMenu()&&upd
		DO CASE
			CASE (PAD()) == UPPER("pad1")
				&&		
			
			CASE (PAD()) == UPPER("pad2")
				IF !MRKBAR(PAD(),BAR())&&upd
					SET MARK OF BAR (BAR()) OF (PAD()) TO .t.&&pad2
				ELSE
					SET MARK OF BAR (BAR()) OF (PAD()) TO .f.&&pad2
				ENDIF		
		ENDCASE	
	ENDPROC
	
	PROCEDURE DoMenu()
	PARAMETERS tnMenu,tnPad,tnBar
	LOCAL lcMenu,lcPad,lnBar
		lcMenu = UPPER(MENU())
		lcPad = UPPER(PAD())
		lnBar = BAR()
		
		FOR EACH loNode IN Application.Objects
			IF ALLTRIM(UPPER(loNode.Class)) == ALLTRIM(UPPER("customform"))
				FOR EACH myobject IN loNode.Controls
					IF ALLTRIM(UPPER(myobject.Name)) == ALLTRIM(UPPER("grid1"))
						loNode.MyAfterRowColChange()
						EXIT
					ENDIF
				ENDFOR
			ENDIF
		ENDFOR
		&&MESSAGEBOX(CNTPAD(MENU()))&&кол-во pad
		&&MESSAGEBOX(CNTBAR(PAD()))&&кол-во bar в pad
		&&MESSAGEBOX(MRKBAR(PAD(),BAR()))&&активный bar в pad
		DO CASE
			CASE tnMenu = 1&&lcMenu = UPPER("menu1")
				DO CASE
					CASE tnPad = 1&&lcPad = UPPER("pad1")&&1
						DO CASE 
							CASE tnBar = 1&&lnBar = 1
								THIS.Menu1Pad1Bar1()
							
							CASE tnBar = 2
								THIS.Menu1Pad1Bar2()
							
							CASE tnBar = 3
								THIS.Menu1Pad1Bar3()
							
							CASE tnBar = 4
								THIS.Menu1Pad1Bar4()
							
							CASE tnBar = 5
						ENDCASE
					
					CASE tnPad = 2&&lcPad = UPPER("pad2")&&2
						IF !MRKBAR(PAD(),BAR())&&upd
							SET MARK OF BAR (tnBar) OF Pad2 TO .t.
							&&
							DO CASE 
								CASE tnBar = 1								
									THIS.Menu1Pad2Bar1()
								
								CASE tnBar = 2
									THIS.Menu1Pad2Bar2()
								
								CASE tnBar = 3
									THIS.Menu1Pad2Bar3()
								
								CASE tnBar = 4
									THIS.Menu1Pad2Bar4()
								
								CASE tnBar = 5
									THIS.Menu1Pad2Bar5()
							ENDCASE			
						ELSE
							SET MARK OF BAR (tnBar) OF Pad2 TO .f.
							&&
							LOCAL lcForm
							DO CASE 
								CASE tnBar = 1								
									lcForm = oModel.FindForm("SelectForm")
								
								CASE tnBar = 2
									lcForm = oModel.FindForm("CheckKdForm")
								
								CASE tnBar = 3									
									lcForm = oModel.FindForm("RequisitesForm")
																									
								CASE tnBar = 4
									lcForm = oModel.FindForm("IzmForm")									
								
								CASE tnBar = 5
									lcForm = oModel.FindForm("PrizForm")									
							ENDCASE
							
							IF VARTYPE(lcForm) == "O"							
								THIS.MarkMenu()
								lcForm.QueryUnload()
							ENDIF	
						ENDIF
																		
					CASE tnPad = 3&&lcPad = UPPER("pad3")&&3 
						IF tnBar = 1
							THIS.Menu1Pad3Bar1()
						ENDIF
						
					CASE tnPad = 4&&lcPad = UPPER("pad4")&&4
						DO CASE 
							CASE tnBar = 1
							CASE tnBar = 2
							CASE tnBar = 3
						ENDCASE
						
					CASE tnPad = 5&&lcPad = UPPER("pad5")&&5
						IF tnBar = 1
						ENDIF
				ENDCASE 
			
			CASE tnMenu = 2&&lcMenu = UPPER("menu2")&&2
		ENDCASE
	ENDPROC
	
	PROCEDURE Menu1Pad1Bar1()
		&&m10870
		
	ENDPROC
	
	PROCEDURE Menu1Pad1Bar2()
		&&m10860
	ENDPROC
	
	PROCEDURE Menu1Pad2Bar1()
		&& Выбрать код		
		oModel_select.Start()
	ENDPROC
	
	PROCEDURE Menu1Pad2Bar2()
		&& Проверить kd
		oModel_ck.Start()
	ENDPROC
	
	PROCEDURE Menu1Pad2Bar3()
		&& Реквизиты
		oModel_req.Start()
	ENDPROC
	
	PROCEDURE Menu1Pad2Bar4()
		&& Корректура
		oModel_izm.Start()
	ENDPROC
	
	PROCEDURE Menu1Pad2Bar5()
		&& Признак
		oModel_priz.Start()
	ENDPROC
	
	PROCEDURE Menu1Pad3Bar1()
	LOCAL lcStr, lcFontName, lcFontSize, lcFontStyle, ;
		lnFind, lnFind1, ;
		lbFontBold, lbFontItalic
		&& Шрифт
		lcFontStyle = ""
		&&
		IF THIS.tbFontBold AND !THIS.tbFontItalic
			lcFontStyle = "B"
		ENDIF
		
		IF !THIS.tbFontBold AND THIS.tbFontItalic
			lcFontStyle = "I"
		ENDIF
		
		IF THIS.tbFontBold AND THIS.tbFontItalic
			lcFontStyle = "BI"
		ENDIF
		&&
		lcStr = GETFONT(ALLTRIM(THIS.tcFontName),THIS.tnFontSize,lcFontStyle)
		&&
		IF !EMPTY(lcStr)
			lnFind = AT(",",lcStr)
			IF lnFind > 0
				lcFontName = SUBSTR(lcStr,1,lnFind-1)
				lnFind1 = lnFind
			ENDIF
			&&
			lnFind = AT(",",lcStr,2)
			IF lnFind > 0
				lcFontSize = SUBSTR(lcStr,lnFind1+1,lnFind-lnFind1-1)
			ENDIF
			&&
			lcFontStyle = SUBSTR(lcStr,lnFind+1)
			lbFontBold = .f.
			lbFontItalic = .f.
			
			DO CASE
				CASE ALLTRIM(lcFontStyle) == "B"
					lbFontBold = .t.
					lbFontItalic = .f.
					
				CASE ALLTRIM(lcFontStyle) == "I"
					lbFontBold = .f.
					lbFontItalic = .t.
					
				CASE ALLTRIM(lcFontStyle) == "BI"
					lbFontBold = .t.
					lbFontItalic = .t.
			ENDCASE
			
			THIS.tcFontName = lcFontName
			THIS.tnFontSize = VAL(lcFontSize)
			THIS.tbFontBold = lbFontBold
			THIS.tbFontItalic = lbFontItalic
			&&
			THIS.SetMyParam("tcFontName",THIS.tcFontName)&& изменение параметров в файле
			THIS.SetMyParam("tnFontSize",THIS.tnFontSize)
			THIS.SetMyParam("tbFontBold",THIS.tbFontBold)
			THIS.SetMyParam("tbFontItalic",THIS.tbFontItalic)
			&&
			THIS.SetCustomFont() 
		ENDIF				
	ENDPROC
	
	PROCEDURE MenuAddBar()
		&&RELEASE POPUPS Pad2
		FOR EACH loNode IN Application.Objects
			DO CASE 
				CASE ALLTRIM(UPPER(loNode.Class)) == ALLTRIM(UPPER("customform"))			
					RELEASE POPUPS Pad2&&upd
					DEFINE POPUP Pad2 MARGIN RELATIVE SHADOW COLOR SCHEME 4
					DEFINE BAR 1 OF Pad2 PROMPT "Выбрать код"
					ON SELECTION BAR 1 OF Pad2 oPath.DoMenu(1,2,1)
					&&
					DEFINE BAR 2 OF Pad2 PROMPT "Проверить код"
					ON SELECTION BAR 2 OF Pad2 oPath.DoMenu(1,2,2)
					&&
					DEFINE BAR 3 OF Pad2 PROMPT "Реквизиты"
					ON SELECTION BAR 3 OF Pad2 oPath.DoMenu(1,2,3)
					&&
					DEFINE BAR 4 OF Pad2 PROMPT "Корректура"
					ON SELECTION BAR 4 OF Pad2 oPath.DoMenu(1,2,4)
					&&
					DEFINE BAR 5 OF Pad2 PROMPT "Признак"
					ON SELECTION BAR 5 OF Pad2 oPath.DoMenu(1,2,5)
					&&
*!*						DEFINE BAR 6 OF Pad2 PROMPT "Повторяющиеся строки"
*!*						ON SELECTION BAR 6 OF Pad2 oPath.DoMenu(1,2,6)
			ENDCASE			
		ENDFOR
	ENDPROC
	
*!*		PROCEDURE Menu1Pad2Bar1()
*!*		LOCAL lcForm, ;
*!*			lnColor		
*!*			&& Цвет строки грида
*!*			lnColor = GETCOLOR()
*!*			IF lnColor >= 0
*!*				THIS.tnGridColor = lnColor
*!*				&&
*!*				FOR EACH loNode IN Application.Objects
*!*					IF ALLTRIM(UPPER(loNode.Class)) == UPPER("grid")
*!*						loNode.HighlightBackColor = lnColor
*!*					ENDIF								 
*!*				ENDFOR	
*!*				&&
*!*				THIS.SetMyParam("tnGridColor",THIS.tnGridColor)
*!*				THIS.SetCustomGridColor()
*!*			ENDIF		
*!*		ENDPROC
*!*		
*!*		PROCEDURE Menu1Pad2Bar2()
*!*		LOCAL lcForm, ;
*!*			lnColor
*!*			&& Цвет текста строки грида
*!*			lnColor = GETCOLOR()
*!*			IF lnColor >= 0
*!*				THIS.tnGridTextColor = lnColor
*!*				&&
*!*				FOR EACH loNode IN Application.Objects 
*!*					IF ALLTRIM(UPPER(loNode.Class)) == UPPER("grid")
*!*						loNode.HighlightForeColor = lnColor
*!*					ENDIF								 
*!*				ENDFOR
*!*				&&
*!*				THIS.SetMyParam("tnGridTextColor",THIS.tnGridTextColor)
*!*				THIS.SetCustomGridTextColor()
*!*			ENDIF		
*!*		ENDPROC
*!*		
*!*		PROCEDURE Menu1Pad2Bar3()
*!*		LOCAL lnColor
*!*			&& Цвет фона главного окна
*!*			lnColor = GETCOLOR()
*!*			IF lnColor >= 0
*!*				THIS.tnScreenBackColor = lnColor
*!*				THIS.SetMyParam("tnScreenBackColor",THIS.tnScreenBackColor)
*!*				THIS.SetCustomScreenBackColor()
*!*			ENDIF		
*!*		ENDPROC
*!*		
*!*		PROCEDURE Menu1Pad3Bar1()
*!*			oModel_help.Start()
*!*		ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: ScreenClass - класс главного окна _SCREEN
DEFINE CLASS ScreenClass AS CUSTOM
	PROCEDURE Init()
		&& Системное меню
		DEFINE MENU Menu1 IN WINDOW (THISFORM.Name) BAR
		DEFINE PAD Pad1 OF Menu1 PROMPT "Таблица" COLOR SCHEME 3
		DEFINE PAD Pad2 OF Menu1 PROMPT "Действия" COLOR SCHEME 3		
		DEFINE PAD Pad3 OF Menu1 PROMPT "Шрифт" COLOR SCHEME 3
		DEFINE PAD Pad4 OF Menu1 PROMPT "Цвет" COLOR SCHEME 3
		DEFINE PAD Pad5 OF Menu1 PROMPT "Помощь" COLOR SCHEME 3
		ON PAD Pad1 OF Menu1 ACTIVATE POPUP Pad1
		ON PAD Pad2 OF Menu1 ACTIVATE POPUP Pad2
		ON PAD Pad3 OF Menu1 ACTIVATE POPUP Pad3
		ON PAD Pad4 OF Menu1 ACTIVATE POPUP Pad4
		ON PAD Pad5 OF Menu1 ACTIVATE POPUP Pad5
		&&
		DEFINE POPUP Pad1 MARGIN RELATIVE SHADOW COLOR SCHEME 4
		DEFINE BAR 1 OF Pad1 PROMPT "1 | M10870"
		ON SELECTION BAR 1 OF Pad1 oPath.DoMenu(1,1,1)&&oPath.Menu1Pad1Bar1()
		&&
		DEFINE BAR 2 OF Pad1 PROMPT "2 | M10860"
		ON SELECTION BAR 2 OF Pad1 oPath.DoMenu(1,1,2)&&oPath.Menu1Pad1Bar2()
		&&
		DEFINE BAR 3 OF Pad1 PROMPT "3 | M10880"
		ON SELECTION BAR 3 OF Pad1 oPath.DoMenu(1,1,3)&&oPath.Menu1Pad1Bar3()
		&&
		DEFINE BAR 4 OF Pad1 PROMPT "4 | M10881"
		ON SELECTION BAR 4 OF Pad1 oPath.DoMenu(1,1,4)&&oPath.Menu1Pad1Bar4()
		&&
		DEFINE BAR 5 OF Pad1 PROMPT "5 | M10881K"
		ON SELECTION BAR 5 OF Pad1 oPath.DoMenu(1,1,5)&&oPath.Menu1Pad1Bar5()
		&&
		DEFINE POPUP Pad3 MARGIN RELATIVE SHADOW COLOR SCHEME 4
		DEFINE BAR 1 OF Pad3 PROMPT "Изменить"
		ON SELECTION BAR 1 OF Pad3 oPath.DoMenu(1,3,1)&&oPath.Menu1Pad3Bar1()
		&&
		DEFINE POPUP Pad4 MARGIN RELATIVE SHADOW COLOR SCHEME 4
		DEFINE BAR 1 OF Pad4 PROMPT "Строка таблицы"
		ON SELECTION BAR 1 OF Pad4 oPath.DoMenu(1,4,1)&&oPath.Menu1Pad4Bar1()
		&&
		DEFINE BAR 2 OF Pad4 PROMPT "Текст строки таблицы"
		ON SELECTION BAR 2 OF Pad4 oPath.DoMenu(1,4,2)&&oPath.Menu1Pad4Bar2()
		&&
		DEFINE BAR 3 OF Pad4 PROMPT "Фон главного окна"
		ON SELECTION BAR 3 OF Pad4 oPath.DoMenu(1,4,3)&&oPath.Menu1Pad4Bar3()
		&&
		DEFINE POPUP Pad5 MARGIN RELATIVE SHADOW COLOR SCHEME 4
		DEFINE BAR 1 OF Pad5 PROMPT "О программе"
		ON SELECTION BAR 1 OF Pad5 oPath.DoMenu(1,5,1)&&oPath.Menu1Pad5Bar1()
		&&
		ACTIVATE MENU Menu1 NOWAIT
	ENDPROC
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& CLASS: FormTemp - custom класс для вычисления параметров объектов формы
DEFINE CLASS FormTemp AS CUSTOM
	tcText = ""
	
	PROCEDURE GetText()
	LOCAL lcText
		lcText = ""
		DO WHILE LEN(lcText) <= 254
			lcText = lcText + SYS(2015)
		ENDDO
		&&
		lcText = SUBSTR(lcText,1,254)
		THIS.tcText = lcText
	ENDPROC
	
	PROCEDURE Init()
		THIS.GetText()
		oPath.SetCustomFont()
	ENDPROC

	ADD OBJECT Label1 AS Label WITH Name = "Label1", ;
		AutoSize = .t., Caption = "", Visible = .t., ;
		FontName = 'Consolas', FontSize = 11
	ADD OBJECT Image1 AS Image WITH Enabled = .t., Picture = "help_1.png", Visible = .t.
ENDDEFINE
&&-------------------------------------------------------------------------------------------------------------------------------
&&-------------------------------------------------------------------------------------------------------------------------------
&& Глобальные процедуры
PROCEDURE FormDelete()
PARAMETERS tcFormName
EXTERNAL ARRAY oPath.taForms
	FOR i = 1 TO ALEN(oPath.taForms,1)
		IF VARTYPE(oPath.taForms[i]) == "O"
			lcForm = oPath.taForms[i]	
			IF ALLTRIM(UPPER(lcForm.Class)) == ALLTRIM(UPPER(tcFormName))
				lcForm = .f.
				oPath.taForms[i] = .f.
			ENDIF
		ENDIF
	ENDFOR
ENDPROC

&& Корректное завершение программы
PROCEDURE MyExit()
PUBLIC poNode
LOCAL loNodeT
	loNodeT = poNode
	FOR EACH poNode IN Application.Objects
		IF UPPER(ALLTRIM(poNode.BaseClass)) == UPPER(ALLTRIM("form"))&&upd
			IF UPPER(ALLTRIM(poNode.Name)) == UPPER("customform")&&upd
				IF PEMSTATUS(poNode,"Unload",5)
					poNode.Unload()														
					IF TYPE("poNode") != "O"
						poNode = loNodeT
					ENDIF					
				ENDIF
			ENDIF
			&&
			FOR EACH myobject IN poNode.Controls
				IF UPPER(ALLTRIM(myobject.Class)) == UPPER(ALLTRIM("grid"))
					&& сохранение в последних редактируемых строках
					poNode.MyAfterRowColChange()
				ENDIF
			ENDFOR
		ENDIF
		&&
		poNode.Destroy()
	ENDFOR
	&&
	RELEASE ALL EXTENDED
	CLEAR EVENTS
	QUIT
ENDPROC