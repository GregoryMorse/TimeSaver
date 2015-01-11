#include <Windows.h>
#include <ScrnSave.h>
#include <tchar.h>
#include <time.h>
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

BOOL WINAPI ScreenSaverConfigureDialog(HWND hDlg, UINT message,
										WPARAM wParam, LPARAM lParam)
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

#define HORZBUF .0375

BOOL CALLBACK DrawTimeSaverProc(
	HMONITOR hMonitor,  // handle to display monitor
	HDC hdc,     // handle to monitor DC
	LPRECT lprcMonitor, // monitor intersection rectangle
	LPARAM data       // data
	)
{
	RECT         rc;       // RECT structure  
	HDC hbdc;
	HBITMAP hbmp;
	HBITMAP holdbmp;
	TCHAR szTime[10];
	HFONT hOldFont;
	TEXTMETRIC txm;
	SIZE pt;
	XFORM x;
	HFONT hFont = (HFONT)data;
	hbdc = CreateCompatibleDC(hdc);
	//GetClientRect(hWnd, &rc);
	rc = *lprcMonitor;
	hbmp = CreateCompatibleBitmap(hdc, rc.right - rc.left, rc.bottom - rc.top);
	holdbmp = (HBITMAP)SelectObject(hbdc, hbmp);
	FillRect(hbdc, &rc, (HBRUSH)GetStockObject(BLACK_BRUSH));
	_tstrtime_s(szTime);
	SetMapMode(hbdc, MM_TEXT);
	SetTextColor(hbdc, RGB(255, 0, 0));
	SetTextAlign(hbdc, TA_TOP | TA_CENTER | TA_NOUPDATECP);
	SetBkColor(hbdc, RGB(0, 0, 0));
	SetGraphicsMode(hbdc, GM_ADVANCED);
	hOldFont = (HFONT)SelectObject(hbdc, hFont);
	GetTextMetrics(hbdc, &txm);
	GetTextExtentPoint32(hbdc, szTime, _tcslen(szTime), &pt);
	x.eM11 = (FLOAT)(rc.right - rc.left) * (1 - HORZBUF * 2) / (FLOAT)pt.cx;
	x.eM22 = (rc.bottom - rc.top) * 9.0f / 8.0f / pt.cy;
	x.eM12 = x.eM21 = x.eDx = x.eDy = 0;
	SetWorldTransform(hbdc, &x);
	rc.left += (int)((FLOAT)(rc.right - rc.left) * HORZBUF);
	rc.right -= (int)((FLOAT)(rc.right - rc.left) * HORZBUF);
	ExtTextOut(hbdc, (int)(((FLOAT)(rc.right - rc.left) / 2 +
		((FLOAT)(rc.right - rc.left) * HORZBUF)) / x.eM11),
		(-txm.tmDescent) / x.eM22, 0, &rc,
				szTime, _tcslen(szTime), NULL);
	
	rc = *lprcMonitor;
	TCHAR szDate[1024] = { 0 };
	time_t t = time(NULL);
	tm _tm;
	localtime_s(&_tm, &t);
	_tcsftime(szDate, 1024, _T("%#x"), &_tm);
	SetTextColor(hbdc, RGB(255, 255, 255));
	GetTextExtentPoint32(hbdc, szDate, _tcslen(szDate), &pt);
	x.eM11 = (FLOAT)(rc.right - rc.left) * (1 - HORZBUF * 2) / (FLOAT)pt.cx;
	x.eM22 = (rc.bottom - rc.top) / 8.0f / pt.cy;
	SetWorldTransform(hbdc, &x);
	rc.left += (int)((FLOAT)(rc.right - rc.left) * HORZBUF);
	rc.right -= (int)((FLOAT)(rc.right - rc.left) * HORZBUF);
	ExtTextOut(hbdc, (int)(((FLOAT)(rc.right - rc.left) / 2 +
		((FLOAT)(rc.right - rc.left) * HORZBUF)) / x.eM11),
		(rc.top + (rc.bottom - rc.top) * 7.0f / 8.0f) / x.eM22, 0, &rc,
		szDate, _tcslen(szDate), NULL);
	SelectObject(hbdc, hOldFont);
	rc.left -= (int)((FLOAT)(rc.right - rc.left) * HORZBUF);
	rc.right += (int)((FLOAT)(rc.right - rc.left) * HORZBUF);
	x.eM11 = 1;
	x.eM22 = 1;
	SetWorldTransform(hbdc, &x);
	BitBlt(hdc, lprcMonitor->left, lprcMonitor->top, rc.right - rc.left, rc.bottom - rc.top,
		hbdc, 0, 0, SRCCOPY);
	SelectObject(hbdc, holdbmp);
	DeleteObject(hbmp);
	DeleteDC(hbdc);
	return TRUE;
}

LRESULT WINAPI ScreenSaverProc(HWND hWnd, UINT message,
								WPARAM wParam, LPARAM lParam)
{
	static HDC          hdc;      // device-context handle  
	static RECT         rc;       // RECT structure  
	static UINT         uTimer;   // timer identifier  
	static HFONT hFont;

	switch (message)
	{
	case WM_CREATE:

		// Retrieve the application name from the .rc file. 
		LoadString(hMainInstance, idsAppName, szAppName,
											APPNAMEBUFFERLEN * sizeof(TCHAR));

		// Retrieve the .ini (or registry) file name. 
		LoadString(hMainInstance, idsIniFile, szIniFile,
												MAXFILELEN * sizeof(TCHAR));

		// TODO: Add error checking to verify LoadString success
		//       for both calls.

		// Retrieve any data from the registry.  
		//GetPrivateProfileInt(szAppName, , , szIniFile);

		// Set a timer for the screen saver window using the 
		// redraw rate stored in Regedit.ini. 
		GetClientRect(hWnd, &rc);
		hFont = CreateFont((rc.bottom - rc.top) / 2, 0, 0, 0, FW_BOLD, FALSE,
			FALSE, FALSE, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
			CLIP_DEFAULT_PRECIS, CLEARTYPE_QUALITY, FIXED_PITCH,
			_T("Microsoft Sans Serif"));
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
		EnumDisplayMonitors(hdc, NULL, DrawTimeSaverProc, (LPARAM)hFont);
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
