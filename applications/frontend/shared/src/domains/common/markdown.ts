export const extractImageUrls = (markdown: string): Set<string> => {
  const regex = /!\[[^\]]*\]\(([^)]+)\)/g;
  const urls = new Set<string>();
  let match;
  while ((match = regex.exec(markdown)) !== null) {
    if (match[1]) {
      urls.add(match[1]);
    }
  }
  return urls;
};
