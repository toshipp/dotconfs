from libqtile.layout.base import Layout
from libqtile import manager

class Wmfs(Layout):
    defaults = manager.Defaults(
        ("border_focus", "#ff0000", "Border colour for the focused window."),
        ("border_normal", "#000000", "Border colour for un-focused winows."),
        ("border_width", 2, "Border width."),
        ("name", "wmfs", "Name of this layout."),
    )

    def __init__(self, **config):
        super(Wmfs, self).__init__(**config)
        self.clients = []
        self.focused = None

    def clone(self, group):
        c = super(Wmfs, self).clone(group)
        c.clients = []
        c.focused = None
        return c

    def focus(self, win):
        self.focused = win

    def add(self, win):
        self.focus(win)
        self.clients.append(win)

    def remove(self, win):
        '''
        This function is called by group.remove,
        and its returen value is used to focus a window.
        But if the value is None, use focus_first to get the value.
        (Maybe it is implemeted specification)
        '''
        i = self.clients.index(win)
        del self.clients[i]
        if self.clients:
            self.focus(self.clients[i % len(self.clients)])
        else:
            self.focus(None)

    def configure(self, win, screen):
        #color = self.group.qtile.colorPixel("#ff0000")
        if win is self._get_window():
            win.place(
                screen.x, screen.y,
                screen.width -1 , screen.height-1,
                1, None)
            win.unhide()
        else:
            win.hide()
    
    def _get_window(self):
        return self.focused

    def focus_first(self):
        '''
        This function is called by group.remove.
        So SHOULD be implemented.
        '''
        return self._get_window()
