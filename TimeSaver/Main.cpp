#include <Windows.h>
#include <ScrnSave.h>
#include <tchar.h>
#include <Strsafe.h>

#include "resource.h"

//#define MINVEL  1                 // minimum redraw speed value     
//#define MAXVEL  10                // maximum redraw speed value    
//#define DEFVEL  5                 // default redraw speed value    
//
//LONG    lSpeed = DEFVEL;          // redraw speed variable         

extern HINSTANCE hMainInstance;   // screen saver instance handle  

//TCHAR   szAppName[APPNAMEBUFFERLEN];             // .ini section name             
//TCHAR   szTemp[20];                // temporary array of characters  
//TCHAR   szRedrawSpeed[] = _T("Redraw Speed");   // .ini speed entry 
//TCHAR   szIniFile[MAXFILELEN];     // .ini or registry file name  

BOOL WINAPI ScreenSaverConfigureDialog(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
	HRESULT  hr;
	static HWND hSpeed;   // handle to speed scroll bar 
	static HWND hOK;      // handle to OK push button  

	switch (message)
	{
	case WM_INITDIALOG:

		// Retrieve the application name from the .rc file.  
		LoadString(hMainInstance, idsAppName, szAppName,
			APPNAMEBUFFERLEN * sizeof(TCHAR));

		// Retrieve the .ini (or registry) file name. 
		LoadString(hMainInstance, idsIniFile, szIniFile,
			MAXFILELEN * sizeof(TCHAR));

		// TODO: Add error checking to verify LoadString success
		//       for both calls.

		// Retrieve any redraw speed data from the registry. 
		//lSpeed = GetPrivateProfileInt(szAppName, szRedrawSpeed,
		//	DEFVEL, szIniFile);

		// If the initialization file does not contain an entry 
		// for this screen saver, use the default value. 
		//if (lSpeed > MAXVEL || lSpeed < MINVEL)
		//	lSpeed = DEFVEL;

		// Initialize the redraw speed scroll bar control.
		/*hSpeed = GetDlgItem(hDlg, ID_SPEED);
		SetScrollRange(hSpeed, SB_CTL, MINVEL, MAXVEL, FALSE);
		SetScrollPos(hSpeed, SB_CTL, lSpeed, TRUE);
		*/
		// Retrieve a handle to the OK push button control.  
		hOK = GetDlgItem(hDlg, ID_OK);

		return TRUE;

	//case WM_HSCROLL:

	//	// Process scroll bar input, adjusting the lSpeed 
	//	// value as appropriate. 
	//	switch (LOWORD(wParam))
	//	{
	//	case SB_PAGEUP:
	//		--lSpeed;
	//		break;

	//	case SB_LINEUP:
	//		--lSpeed;
	//		break;

	//	case SB_PAGEDOWN:
	//		++lSpeed;
	//		break;

	//	case SB_LINEDOWN:
	//		++lSpeed;
	//		break;

	//	case SB_THUMBPOSITION:
	//		lSpeed = HIWORD(wParam);
	//		break;

	//	case SB_BOTTOM:
	//		lSpeed = MINVEL;
	//		break;

	//	case SB_TOP:
	//		lSpeed = MAXVEL;
	//		break;

	//	case SB_THUMBTRACK:
	//	case SB_ENDSCROLL:
	//		return TRUE;
	//		break;
	//	}

	//	if ((int)lSpeed <= MINVEL)
	//		lSpeed = MINVEL;
	//	if ((int)lSpeed >= MAXVEL)
	//		lSpeed = MAXVEL;

	//	SetScrollPos((HWND)lParam, SB_CTL, lSpeed, TRUE);
	//	break;

	case WM_COMMAND:
		switch (LOWORD(wParam))
		{
		case ID_OK:

			// Write the current redraw speed variable to
			// the .ini file. 
			/*hr = StringCchPrintf(szTemp, 20, _T("%ld"), lSpeed);
			if (SUCCEEDED(hr))
				WritePrivateProfileString(szAppName, szRedrawSpeed,
				szTemp, szIniFile);*/

		case ID_CANCEL:
			EndDialog(hDlg, LOWORD(wParam) == ID_OK);

			return TRUE;
		}
	}
	return FALSE;
}

BOOL WINAPI RegisterDialogClasses(HANDLE hInst)
{
	return TRUE;
}

LRESULT WINAPI ScreenSaverProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	static HDC          hdc;      // device-context handle  
	static RECT         rc;       // RECT structure  
	static UINT         uTimer;   // timer identifier  
	static HFONT hFont;
	TCHAR szTime[10];
	HFONT hOldFont;
	XFORM x;

	switch (message)
	{
	case WM_CREATE:

		// Retrieve the application name from the .rc file. 
		LoadString(hMainInstance, idsAppName, szAppName, APPNAMEBUFFERLEN * sizeof(TCHAR));

		// Retrieve the .ini (or registry) file name. 
		LoadString(hMainInstance, idsIniFile, szIniFile, MAXFILELEN * sizeof(TCHAR));

		// TODO: Add error checking to verify LoadString success
		//       for both calls.

		// Retrieve any data from the registry.  
		//GetPrivateProfileInt(szAppName, , , szIniFile);

		// Set a timer for the screen saver window using the 
		// redraw rate stored in Regedit.ini. 
		GetClientRect(hWnd, &rc);
		hFont = CreateFont(rc.bottom / 2, 0, 0, 0, FW_BOLD, FALSE, FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY, FIXED_PITCH, _T("Microsoft Sans Serif"));
		uTimer = SetTimer(hWnd, 1, 500, NULL);

		break;

	case WM_ERASEBKGND:

		// The WM_ERASEBKGND message is issued before the 
		// WM_TIMER message, allowing the screen saver to 
		// paint the background as appropriate. 

		hdc = GetDC(hWnd);
		GetClientRect(hWnd, &rc);
		FillRect(hdc, &rc, (HBRUSH)GetStockObject(BLACK_BRUSH));
		ReleaseDC(hWnd, hdc);
		break;

	case WM_TIMER:

		// The WM_TIMER message is issued at (lSpeed * 1000) 
		// intervals, where lSpeed == .001 seconds. This 
		// code repaints the entire desktop with a white, 
		// light gray, dark gray, or black brush each 
		// time a WM_TIMER message is issued. 

		hdc = GetDC(hWnd);
		GetClientRect(hWnd, &rc);
		//FillRect(hdc, &rc, (HBRUSH)GetStockObject(BLACK_BRUSH));
		_tstrtime_s(szTime);
		SetTextColor(hdc, RGB(255, 0, 0));
		SetTextAlign(hdc, TA_BASELINE | TA_CENTER | TA_NOUPDATECP);
		SetBkColor(hdc, RGB(0, 0, 0));
		SetGraphicsMode(hdc, GM_ADVANCED);
		x.eM11 = 1;
		x.eM22 = 2;
		x.eM12 = x.eM21 = x.eDx = x.eDy = 0;
		SetWorldTransform(hdc, &x);
		hOldFont = (HFONT)SelectObject(hdc, hFont);
		ExtTextOut(hdc, rc.right / 2, rc.bottom / 2  - rc.bottom / 12, 0, &rc, szTime, _tcslen(szTime), NULL);
		SelectObject(hdc, hOldFont);
		ReleaseDC(hWnd, hdc);
		break;

	case WM_DESTROY:

		// When the WM_DESTROY message is issued, the screen saver 
		// must destroy any of the timers that were set at WM_CREATE 
		// time. 
		if (hFont) DeleteObject(hFont);
		if (uTimer)
			KillTimer(hWnd, uTimer);
		break;
	}

	// DefScreenSaverProc processes any messages ignored by ScreenSaverProc. 
	return DefScreenSaverProc(hWnd, message, wParam, lParam);
}
