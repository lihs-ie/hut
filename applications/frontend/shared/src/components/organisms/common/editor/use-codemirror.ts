"use client";

import { useRef, useEffect, useCallback } from "react";
import { EditorView, keymap, placeholder as placeholderExtension } from "@codemirror/view";
import { EditorState } from "@codemirror/state";
import { defaultKeymap, historyKeymap, history, indentWithTab } from "@codemirror/commands";
import { markdown, markdownLanguage } from "@codemirror/lang-markdown";
import { languages } from "@codemirror/language-data";

export type UseCodeMirrorProps = {
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
  onSave?: () => void;
  onPaste?: (event: ClipboardEvent) => void;
  onDrop?: (files: File[]) => void;
};

export type UseCodeMirrorReturn = {
  containerRef: React.RefObject<HTMLDivElement | null>;
  insertText: (text: string) => void;
  replaceText: (searchText: string, replacement: string) => void;
  focus: () => void;
};

export const useCodeMirror = (props: UseCodeMirrorProps): UseCodeMirrorReturn => {
  const containerRef = useRef<HTMLDivElement>(null);
  const viewRef = useRef<EditorView | null>(null);
  const onChangeRef = useRef(props.onChange);
  const onSaveRef = useRef(props.onSave);
  const onPasteRef = useRef(props.onPaste);
  const onDropRef = useRef(props.onDrop);
  const initialValueRef = useRef(props.value);
  const placeholderRef = useRef(props.placeholder);
  const isComposingRef = useRef(false);

  useEffect(() => {
    onChangeRef.current = props.onChange;
  }, [props.onChange]);

  useEffect(() => {
    onSaveRef.current = props.onSave;
  }, [props.onSave]);

  useEffect(() => {
    onPasteRef.current = props.onPaste;
  }, [props.onPaste]);

  useEffect(() => {
    onDropRef.current = props.onDrop;
  }, [props.onDrop]);

  useEffect(() => {
    if (!containerRef.current) return;

    const saveKeymap = keymap.of([
      {
        key: "Mod-s",
        run: () => {
          onSaveRef.current?.();
          return true;
        },
      },
    ]);

    const boldKeymap = keymap.of([
      {
        key: "Mod-b",
        run: (view) => {
          wrapSelectionInView(view, "**", "太字");
          return true;
        },
      },
    ]);

    const italicKeymap = keymap.of([
      {
        key: "Mod-i",
        run: (view) => {
          wrapSelectionInView(view, "*", "イタリック");
          return true;
        },
      },
    ]);

    const extensions = [
      history(),
      markdown({ base: markdownLanguage, codeLanguages: languages }),
      EditorView.lineWrapping,
      keymap.of([...defaultKeymap, ...historyKeymap, indentWithTab]),
      saveKeymap,
      boldKeymap,
      italicKeymap,
      EditorView.updateListener.of((update) => {
        if (update.docChanged && !isComposingRef.current) {
          onChangeRef.current(update.state.doc.toString());
        }
      }),
      EditorView.domEventHandlers({
        compositionstart: () => {
          isComposingRef.current = true;
          return false;
        },
        compositionend: (_event, view) => {
          isComposingRef.current = false;
          onChangeRef.current(view.state.doc.toString());
          return false;
        },
        paste: (event) => {
          const handler = onPasteRef.current;
          if (!handler) return false;
          const items = event.clipboardData?.items;
          if (!items) return false;
          const hasImage = Array.from(items).some((item) =>
            item.type.startsWith("image/"),
          );
          if (!hasImage) return false;
          handler(event);
          return true;
        },
        drop: (event) => {
          const handler = onDropRef.current;
          if (!handler) return false;
          const files = event.dataTransfer?.files;
          if (!files) return false;
          const imageFiles = Array.from(files).filter((file) =>
            file.type.startsWith("image/"),
          );
          if (imageFiles.length === 0) return false;
          event.preventDefault();
          handler(imageFiles);
          return true;
        },
      }),
    ];

    if (placeholderRef.current) {
      extensions.push(placeholderExtension(placeholderRef.current));
    }

    const state = EditorState.create({
      doc: initialValueRef.current,
      extensions,
    });

    const view = new EditorView({
      state,
      parent: containerRef.current,
    });

    viewRef.current = view;

    return () => {
      view.destroy();
      viewRef.current = null;
    };
  }, []);

  useEffect(() => {
    const view = viewRef.current;
    if (!view) return;

    const currentValue = view.state.doc.toString();
    if (currentValue !== props.value) {
      view.dispatch({
        changes: {
          from: 0,
          to: currentValue.length,
          insert: props.value,
        },
      });
    }
  }, [props.value]);

  const insertText = useCallback((text: string) => {
    const view = viewRef.current;
    if (!view) return;

    const { from, to } = view.state.selection.main;
    view.dispatch({
      changes: { from, to, insert: text },
      selection: { anchor: from + text.length },
    });
    view.focus();
  }, []);

  const replaceText = useCallback((searchText: string, replacement: string) => {
    const view = viewRef.current;
    if (!view) return;

    const content = view.state.doc.toString();
    const index = content.indexOf(searchText);
    if (index === -1) return;

    view.dispatch({
      changes: { from: index, to: index + searchText.length, insert: replacement },
    });
  }, []);

  const focus = useCallback(() => {
    viewRef.current?.focus();
  }, []);

  return { containerRef, insertText, replaceText, focus };
};

const wrapSelectionInView = (
  view: EditorView,
  wrapper: string,
  placeholder: string,
): void => {
  const { from, to } = view.state.selection.main;
  const selectedText = view.state.sliceDoc(from, to);

  if (selectedText) {
    const isWrapped =
      selectedText.startsWith(wrapper) && selectedText.endsWith(wrapper);
    if (isWrapped) {
      const unwrapped = selectedText.slice(wrapper.length, -wrapper.length);
      view.dispatch({
        changes: { from, to, insert: unwrapped },
        selection: { anchor: from, head: from + unwrapped.length },
      });
    } else {
      const wrapped = `${wrapper}${selectedText}${wrapper}`;
      view.dispatch({
        changes: { from, to, insert: wrapped },
        selection: { anchor: from, head: from + wrapped.length },
      });
    }
  } else {
    const wrapped = `${wrapper}${placeholder}${wrapper}`;
    view.dispatch({
      changes: { from, to, insert: wrapped },
      selection: {
        anchor: from + wrapper.length,
        head: from + wrapper.length + placeholder.length,
      },
    });
  }
};
