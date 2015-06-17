from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np
import matplotlib
matplotlib.use('Agg') # Disable the display, does not affect graph generation
from mpl_toolkits.axes_grid1 import AxesGrid

import constants

# Set font, globally
default_font = {
    #'family': 'normal',
    'weight' : 'semibold',
    'size' : 20,
    #'linespacing' : 0.4,
}
title_font = {
    #'family': 'normal',
    'weight' : 'semibold',
    'size' : 24,
    #'linespacing' : 0.4,
}
matplotlib.rc('font', **default_font)

#############################################################################

# http://stackoverflow.com/questions/7404116/defining-the-midpoint-of-a-colormap-in-matplotlib
def shiftedColorMap(cmap, start=0, midpoint=0.5, stop=1.0, name='shiftedcmap'):
    '''
    Function to offset the "center" of a colormap. Useful for
    data with a negative min and positive max and you want the
    middle of the colormap's dynamic range to be at zero

    Input
    -----
      cmap : The matplotlib colormap to be altered
      start : Offset from lowest point in the colormap's range.
          Defaults to 0.0 (no lower ofset). Should be between
          0.0 and `midpoint`.
      midpoint : The new center of the colormap. Defaults to 
          0.5 (no shift). Should be between 0.0 and 1.0. In
          general, this should be  1 - vmax/(vmax + abs(vmin))
          For example if your data range from -15.0 to +5.0 and
          you want the center of the colormap at 0.0, `midpoint`
          should be set to  1 - 5/(5 + 15)) or 0.75
      stop : Offset from highets point in the colormap's range.
          Defaults to 1.0 (no upper ofset). Should be between
          `midpoint` and 1.0.
    '''
    cdict = {
        'red': [],
        'green': [],
        'blue': [],
        'alpha': []
    }

    # regular index to compute the colors
    reg_index = np.linspace(start, stop, 257)

    # shifted index to match the data
    shift_index = np.hstack([
        np.linspace(0.0, midpoint, 128, endpoint=False), 
        np.linspace(midpoint, 1.0, 129, endpoint=True)
    ])

    for ri, si in zip(reg_index, shift_index):
        r, g, b, a = cmap(ri)

        cdict['red'].append((si, r, r))
        cdict['green'].append((si, g, g))
        cdict['blue'].append((si, b, b))
        cdict['alpha'].append((si, a, a))

    newcmap = matplotlib.colors.LinearSegmentedColormap(name, cdict)
    plt.register_cmap(cmap=newcmap)

    return newcmap

# For info on colormaps, see
# http://matplotlib.org/api/pyplot_summary.html?highlight=colormaps#matplotlib.pyplot.colormaps
def contour(xbounds, ybounds, zfun, title, xlabel=None, ylabel=None, zlabel=None, samples=constants.GRAPH_SAMPLES, output=None, zlim=None):
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    X,Y = np.meshgrid(np.linspace(xbounds[0], xbounds[1], num=samples)
                      ,np.linspace(ybounds[0], ybounds[1], num=samples))
    # Apply zfun to every point on the surface
    Z = np.vectorize(zfun)(X, Y)
    # cm.coolwarm_r
    # cm.cubehelix
    # cm.CMRmap
    cmap = shiftedColorMap(cm.cubehelix#CMRmap
                           ,start=0, midpoint=0.4, stop=.9, name='shiftedcmap')
    surf = ax.plot_surface(X, Y, Z
                           ,rstride=1, cstride=1 ## TODO, not sure why we need these
                           ,cmap=cmap
                           ,vmin=0, vmax=zlim
                           ,linewidth=0, antialiased=False)
    # Override the x and y ticks
    xposns = list(range(xbounds[0], xbounds[1]+1))
    yposns = list(range(ybounds[0]+2, ybounds[1]+1, 2))
    plt.xticks(xposns, ["%sx" % n for n in xposns])
    plt.yticks(yposns)#, ["%sx" % m for m in yposns])
    # Set the title and z-axis label (z label is a hack)
    plt.suptitle(title, fontdict=title_font, y=0.88)
    plt.title(zlabel, fontdict=default_font, x=0.1, y=0.87)
    #
    ax.set_xlim(xbounds[0], xbounds[1])
    ax.set_ylim(ybounds[0], ybounds[1])
    ax.set_zlim(0, zlim)
    ax.set_xlabel(xlabel, fontdict=default_font)
    ax.set_ylabel(ylabel, fontdict=default_font)
    # Save
    ax.view_init(elev=10, azim=240)
    plt.savefig(output)
    plt.clf()
    plt.close()
    print("Saved contour to '%s'" % output)
    return output

def make_figs(output):
    """
        Produce a ton of views of the same graph,
        Return a list of all views
    """
    figs = []
    for e in range(10, 90, 30):
        for r in range(0, 360, 20):
            out = "%s-%se-%sr.png" % (output, e, r)
            ax.view_init(elev=e, azim=r)
            plt.savefig(out)
            figs.append(out)
    return figs
