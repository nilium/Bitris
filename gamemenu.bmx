
SuperStrict

Import "game.bmx"
Import "menu.bmx"
Import "settings.bmx"
Import brl.ramstream
Import brl.freetypefont
Import brl.standardio

Type btMenuState Extends btState
	
	Field _Menu:btMenu
	Field _Settings:btMenu
	Field _Font:TImageFont
	Method Init:btMenuState()
	
		Local scale:Float = btSettings.Height / 500.0
		
		_Menu = New btMenu
		_Menu.Initialise(GameMenuCallback, Self)
		_Menu.SetPosition(30 * scale, 40 * scale)
		_Menu.SetSpacing(20 * scale)
		_Menu.AddItem("Single-player Standard")
		_Menu.AddItem("Single-player Survival")
		_Menu.AddItem("Single-player Custom")
		_Menu.AddItem("Hotseat")
		_Menu.AddItem("Settings")
		_Menu.AddItem("")
		_Menu.AddItem("Exit")
		
		_Settings = New btMenu
		_Settings.SetCallback(GameMenuCallback)
		_Settings.SetContext(Self)
		_Settings.SetPosition(30 * scale, 40 * scale)
		_Settings.SetSpacing(20.0 * scale)
		
		If btSettings.Glass
			_Settings.AddItem("Glass: On")
		Else
			_Settings.AddItem("Glass: Off")
		End If
		_Settings.AddItem("Fade time: " + Int(btSettings.FadeTime))
		_Settings.AddItem("Pieces: " + btSettings.Moves)
		_Settings.AddItem("Grid width: " + btSettings.GameWidth)
		_Settings.AddItem("Grid height: " + btSettings.GameHeight)
		If btSettings.Skips = -1
			_Settings.AddItem("Skips: Infinate")
		Else
			_Settings.AddItem("Skips: " + btSettings.Skips)
		EndIf
		If btSettings.Seed = 0
			_Settings.AddItem("Random Seed: Random")
		Else
			_Settings.AddItem("Random Seed: " + btSettings.Seed)
		EndIf
		_Settings.AddItem("")
		_Settings.AddItem("Return to main menu")
		
		
		_Font = LoadImageFont("incbin::DroidSans-Bold.ttf", Int(22 * scale))
		
		Return Self
	End Method
	
	Method Run()
		If KeyHit(KEY_ESCAPE)
			If _Settings.IsEnabled()
				_Settings.Disable()
				_Menu.Enable()
			Else
				End
			EndIf
		End If
	End Method
	
	Method MenuClicked(index:Int, text:String, button:Int, Menu:btMenu)
		If Menu = _Menu
			If Button = MOUSE_LEFT
				Select index
					Case 0
						Local g:btGame = New btStandardGame.Init("Standard game", 5, 5)
						g.SetSkipLimit(- 1)
						g.SetTurnsLimit(35)
						CurrentState = g
						Leave()
					Case 1
						Local g:btGame = New btStandardGame.Init("Survival game", 5, 5)
						g.SetSkipLimit(0)
						g.SetTurnsLimit(- 1)
						CurrentState = g
						Leave()
					Case 2
						CurrentState = New btStandardGame.Init("Custom game", btSettings.GameWidth, btSettings.GameHeight, 1, btSettings.Seed)
						Leave()
					Case 3
						Leave()
						Local g:btGame = New btHotseatGame.Init("Hotseat game", btSettings.GameWidth, btSettings.GameHeight, 2, btSettings.Seed)
						g.SetTurnsLimit(btSettings.Moves * 2)
						CurrentState = g
					Case 4
						_Menu.Disable()
						_Settings.Enable()
					Case 6
						End
				End Select
			EndIf
		ElseIf Menu = _Settings
			Select index
				Case 0
					If btSettings.ToggleGlass()
						_Settings.ChangeItem(0, "Glass: On")
					Else
						_Settings.ChangeItem(0, "Glass: Off")
					End If
				Case 1
					If Button = MOUSE_LEFT
						btSettings.SetFadeTime(btSettings.FadeTime + 4)
					ElseIf Button = MOUSE_RIGHT
						btSettings.SetFadeTime(btSettings.FadeTime - 4)
					End If
					_Settings.ChangeItem(1, "Fade time: " + Int(btSettings.FadeTime))
				Case 2
					Local moves:Int
					If Button = MOUSE_LEFT
						moves = btSettings.SetMoves(btSettings.Moves + 1)
					ElseIf Button = MOUSE_RIGHT
						moves = btSettings.SetMoves(btSettings.Moves - 1)
					End If
					If moves = -1
						_Settings.ChangeItem(2, "Pieces: Infinate")
					Else
						_Settings.ChangeItem(2, "Pieces: " + Int(btSettings.Moves))
					EndIf
				Case 3
					If Button = MOUSE_LEFT
						btSettings.SetGameWidth(btSettings.GameWidth + 1)
					ElseIf Button = MOUSE_RIGHT
						btSettings.SetGameWidth(btSettings.GameWidth - 1)
					End If
					_Settings.ChangeItem(3, "Grid width: " + Int(btSettings.GameWidth))
				Case 4
					If Button = MOUSE_LEFT
						btSettings.SetGameHeight(btSettings.GameHeight + 1)
					ElseIf Button = MOUSE_RIGHT
						btSettings.SetGameHeight(btSettings.GameHeight - 1)
					End If
					_Settings.ChangeItem(4, "Grid height: " + Int(btSettings.GameHeight))
				Case 5
					If Button = MOUSE_LEFT
						btSettings.SetSkips(btSettings.Skips + 1)
					ElseIf Button = MOUSE_RIGHT
						btSettings.SetSkips(btSettings.Skips - 1)
					End If
					If btSettings.Skips = -1
						_Settings.ChangeItem(5, "Skips: Infinate")
					Else
						_Settings.ChangeItem(5, "Skips: " + btSettings.Skips)
					EndIf					
				Case 6
					Local seed:Int
					If Button = MOUSE_LEFT
						seed = btSettings.SetSeed(btSettings.Seed + 1)
					ElseIf Button = MOUSE_RIGHT
						seed = btSettings.SetSeed(Max(btSettings.Seed - 1, 0))
					End If
					If seed = 0
						_Settings.ChangeItem(6, "Random Seed: Random")
					Else
						_Settings.ChangeItem(6, "Random Seed: " + btSettings.Seed)
					EndIf
				Case 8
					If Button = MOUSE_LEFT
						_Settings.Disable()
						_Menu.Enable()
					EndIf
			EndSelect
		EndIf
	End Method

	Method Leave()
		_Menu.Disable()
		_Settings.Disable()
	End Method
	
	Method Render()
		SetImageFont(_Font)
		SetColor(0, 0, 0)
		SetScale(1, 1)
		If _Menu.IsEnabled() _Menu.Render()
		If _Settings.IsEnabled() _Settings.Render()
		SetColor(200, 200, 200)
		SetScale(0.5, 0.5)
		DrawText("Quadris is a Clone of Bitris (by Katharine Berry)", 10, btSettings.Height - 38)
		DrawText("   Programmed by Joseph Atkins-Turkish", 10, btSettings.Height - 22)
		SetScale(1, 1)
	End Method
	
End Type

Function GameMenuCallback(index:Int, text:String, Button:Int, Menu:btMenu, context:Object)
	btMenuState(context).MenuClicked(index, Text, Button, Menu)
EndFunction