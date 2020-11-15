const rootStyles = window.getComputedStyle(document.documentElement);

if (rootStyles.getPropertyValue('--book-cover-width-large') != null && ('--book-cover-width-large') != '') {
  ready();
} else {
  document.getElementById('main-css').addEventListener('load', ready);
}

function ready() {
  const coverWidth = parseFloat(rootStyles.getPropertyValue('--book-cover-width-large'));
  const coverRatio = parseFloat(rootStyles.getPropertyValue('--book-cover-aspect-ratio'));
  const coverHeight = coverWidth / coverRatio;

  FilePond.registerPlugin(
    FilePondPluginImagePreview,
    FilePondPluginImageResize,
    FilePondPluginFileEncode
  )

  FilePond.setOptions({
    stylePanelAspectRatio: 1 / coverRatio,
    imageResizeTargetWidth: coverWidth,
    imageResizeTargetHeight: coverHeight
  })

  FilePond.parse(document.body);
}