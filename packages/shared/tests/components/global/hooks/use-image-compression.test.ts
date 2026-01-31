/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import {
  compressImageToWebP,
  getCompressedFileName,
  calculateResizedDimensions,
  DEFAULT_COMPRESSION_OPTIONS,
} from "@shared/components/global/hooks/use-image-compression";

class MockImage {
  width: number;
  height: number;
  onload: (() => void) | null = null;
  onerror: (() => void) | null = null;
  private _src = "";

  constructor(width = 100, height = 100) {
    this.width = width;
    this.height = height;
  }

  get src() {
    return this._src;
  }

  set src(value: string) {
    this._src = value;
    setTimeout(() => {
      if (this.onload) this.onload();
    }, 0);
  }
}

class MockImageFailing {
  width = 0;
  height = 0;
  onload: (() => void) | null = null;
  onerror: (() => void) | null = null;
  private _src = "";

  get src() {
    return this._src;
  }

  set src(value: string) {
    this._src = value;
    setTimeout(() => {
      if (this.onerror) this.onerror();
    }, 0);
  }
}

const createMockCanvas = (returnNullContext = false) => {
  const mockContext = {
    drawImage: vi.fn(),
  };

  let canvasWidth = 0;
  let canvasHeight = 0;

  const mockCanvas = {
    get width() {
      return canvasWidth;
    },
    set width(value: number) {
      canvasWidth = value;
    },
    get height() {
      return canvasHeight;
    },
    set height(value: number) {
      canvasHeight = value;
    },
    getContext: vi.fn().mockReturnValue(returnNullContext ? null : mockContext),
    toBlob: vi.fn(
      (
        callback: (blob: Blob | null) => void,
        mimeType: string,
        _quality: number
      ) => {
        const blob = new Blob(["test"], { type: mimeType });
        callback(blob);
      }
    ),
  };

  return {
    mockCanvas,
    mockContext,
    getWidth: () => canvasWidth,
    getHeight: () => canvasHeight,
  };
};

describe("hooks/useImageCompression", () => {
  describe("calculateResizedDimensions", () => {
    it("maxWidth以内の画像はそのまま", () => {
      const result = calculateResizedDimensions(400, 300, 800, 600);
      expect(result.width).toBe(400);
      expect(result.height).toBe(300);
    });

    it("幅がmaxWidthを超える場合はリサイズ", () => {
      const result = calculateResizedDimensions(1600, 1200, 800, 600);
      expect(result.width).toBe(800);
      expect(result.height).toBe(600);
    });

    it("高さがmaxHeightを超える場合はリサイズ", () => {
      const result = calculateResizedDimensions(400, 900, 800, 600);
      expect(result.width).toBe(267);
      expect(result.height).toBe(600);
    });

    it("両方超える場合は最初に幅を調整し、次に高さを調整", () => {
      const result = calculateResizedDimensions(2000, 1500, 800, 600);
      expect(result.width).toBe(800);
      expect(result.height).toBe(600);
    });

    it("アスペクト比が維持される", () => {
      const result = calculateResizedDimensions(1600, 1200, 800, 800);
      const originalRatio = 1600 / 1200;
      const resultRatio = result.width / result.height;
      expect(Math.abs(originalRatio - resultRatio)).toBeLessThan(0.01);
    });
  });

  describe("getCompressedFileName", () => {
    it("GIFの場合は元のファイル名を返す", () => {
      expect(getCompressedFileName("animation.gif", "image/gif")).toBe(
        "animation.gif"
      );
    });

    it("JPEGの場合はWebP拡張子に変更する", () => {
      expect(getCompressedFileName("photo.jpg", "image/jpeg")).toBe(
        "photo.webp"
      );
      expect(getCompressedFileName("photo.jpeg", "image/jpeg")).toBe(
        "photo.webp"
      );
    });

    it("PNGの場合はWebP拡張子に変更する", () => {
      expect(getCompressedFileName("screenshot.png", "image/png")).toBe(
        "screenshot.webp"
      );
    });

    it("WebPの場合はそのままWebP拡張子", () => {
      expect(getCompressedFileName("image.webp", "image/webp")).toBe(
        "image.webp"
      );
    });

    it("複数のドットを含むファイル名でも正しく処理される", () => {
      expect(getCompressedFileName("my.photo.2024.jpg", "image/jpeg")).toBe(
        "my.photo.2024.webp"
      );
    });
  });

  describe("DEFAULT_COMPRESSION_OPTIONS", () => {
    it("デフォルト値が設定されている", () => {
      expect(DEFAULT_COMPRESSION_OPTIONS.maxWidth).toBe(1200);
      expect(DEFAULT_COMPRESSION_OPTIONS.maxHeight).toBe(1200);
      expect(DEFAULT_COMPRESSION_OPTIONS.quality).toBe(0.8);
    });
  });

  describe("compressImageToWebP", () => {
    let originalCreateElement: typeof document.createElement;
    let originalImage: typeof globalThis.Image;
    let originalCreateObjectURL: typeof URL.createObjectURL;
    let originalRevokeObjectURL: typeof URL.revokeObjectURL;

    beforeEach(() => {
      originalCreateElement = document.createElement.bind(document);
      originalImage = globalThis.Image;
      originalCreateObjectURL = URL.createObjectURL;
      originalRevokeObjectURL = URL.revokeObjectURL;

      URL.createObjectURL = vi.fn().mockReturnValue("blob:mock-url");
      URL.revokeObjectURL = vi.fn();
    });

    afterEach(() => {
      document.createElement = originalCreateElement;
      globalThis.Image = originalImage;
      URL.createObjectURL = originalCreateObjectURL;
      URL.revokeObjectURL = originalRevokeObjectURL;
      vi.clearAllMocks();
    });

    it("GIFファイルは圧縮せずそのまま返す", async () => {
      const gifFile = new File([new ArrayBuffer(100)], "test.gif", {
        type: "image/gif",
      });

      const compressed = await compressImageToWebP(gifFile).unwrap();

      expect(compressed.mimeType).toBe("image/gif");
      expect(compressed.blob).toBe(gifFile);
      expect(compressed.originalSize).toBe(100);
      expect(compressed.compressedSize).toBe(100);
    });

    it("GIF以外のファイルはWebPに変換される", async () => {
      const { mockCanvas, mockContext } = createMockCanvas();

      document.createElement = vi.fn((tagName: string) => {
        if (tagName === "canvas") {
          return mockCanvas as unknown as HTMLCanvasElement;
        }
        return originalCreateElement(tagName);
      }) as typeof document.createElement;

      globalThis.Image = MockImage as unknown as typeof Image;

      const pngFile = new File([new ArrayBuffer(1000)], "test.png", {
        type: "image/png",
      });

      const compressed = await compressImageToWebP(pngFile).unwrap();

      expect(compressed.mimeType).toBe("image/webp");
      expect(mockContext.drawImage).toHaveBeenCalled();
      expect(mockCanvas.toBlob).toHaveBeenCalled();
    });

    it("大きな画像はmaxWidth/maxHeight以内にリサイズされる", async () => {
      const { mockCanvas, getWidth, getHeight } = createMockCanvas();

      document.createElement = vi.fn((tagName: string) => {
        if (tagName === "canvas") {
          return mockCanvas as unknown as HTMLCanvasElement;
        }
        return originalCreateElement(tagName);
      }) as typeof document.createElement;

      class LargeImage extends MockImage {
        constructor() {
          super(2000, 1500);
        }
      }

      globalThis.Image = LargeImage as unknown as typeof Image;

      const jpegFile = new File([new ArrayBuffer(5000)], "large.jpg", {
        type: "image/jpeg",
      });

      const compressed = await compressImageToWebP(jpegFile, {
        maxWidth: 800,
        maxHeight: 600,
        quality: 0.8,
      }).unwrap();

      expect(compressed.width).toBeLessThanOrEqual(800);
      expect(compressed.height).toBeLessThanOrEqual(600);
      expect(getWidth()).toBeLessThanOrEqual(800);
      expect(getHeight()).toBeLessThanOrEqual(600);
    });

    it("アスペクト比が維持される", async () => {
      const { mockCanvas } = createMockCanvas();

      document.createElement = vi.fn((tagName: string) => {
        if (tagName === "canvas") {
          return mockCanvas as unknown as HTMLCanvasElement;
        }
        return originalCreateElement(tagName);
      }) as typeof document.createElement;

      class WideImage extends MockImage {
        constructor() {
          super(1600, 1200);
        }
      }

      globalThis.Image = WideImage as unknown as typeof Image;

      const pngFile = new File([new ArrayBuffer(2000)], "wide.png", {
        type: "image/png",
      });

      const compressed = await compressImageToWebP(pngFile, {
        maxWidth: 800,
        maxHeight: 800,
        quality: 0.8,
      }).unwrap();

      const aspectRatio = compressed.width / compressed.height;
      const originalAspectRatio = 1600 / 1200;
      expect(Math.abs(aspectRatio - originalAspectRatio)).toBeLessThan(0.01);
    });

    it("maxWidth/maxHeight未満の画像はそのままのサイズで出力される", async () => {
      const { mockCanvas } = createMockCanvas();

      document.createElement = vi.fn((tagName: string) => {
        if (tagName === "canvas") {
          return mockCanvas as unknown as HTMLCanvasElement;
        }
        return originalCreateElement(tagName);
      }) as typeof document.createElement;

      class SmallImage extends MockImage {
        constructor() {
          super(200, 150);
        }
      }

      globalThis.Image = SmallImage as unknown as typeof Image;

      const pngFile = new File([new ArrayBuffer(500)], "small.png", {
        type: "image/png",
      });

      const compressed = await compressImageToWebP(pngFile).unwrap();

      expect(compressed.width).toBe(200);
      expect(compressed.height).toBe(150);
    });

    it("Canvas contextが取得できない場合はエラーを返す", async () => {
      const { mockCanvas } = createMockCanvas(true);

      document.createElement = vi.fn((tagName: string) => {
        if (tagName === "canvas") {
          return mockCanvas as unknown as HTMLCanvasElement;
        }
        return originalCreateElement(tagName);
      }) as typeof document.createElement;

      globalThis.Image = MockImage as unknown as typeof Image;

      const pngFile = new File([new ArrayBuffer(500)], "test.png", {
        type: "image/png",
      });

      const error = await compressImageToWebP(pngFile).unwrapError();

      expect(error.field).toBe("canvas");
    });

    it("画像読み込みが失敗した場合はエラーを返す", async () => {
      globalThis.Image = MockImageFailing as unknown as typeof Image;

      const pngFile = new File([new ArrayBuffer(500)], "test.png", {
        type: "image/png",
      });

      const error = await compressImageToWebP(pngFile).unwrapError();

      expect(error.field).toBe("compression");
    });
  });
});
