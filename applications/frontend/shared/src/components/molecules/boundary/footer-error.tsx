"use client";

import { Component, ReactNode } from "react";
import { FooterPresenter } from "@shared/components/organisms/footer/index.presenter";

type Props = {
  children: ReactNode;
};

type State = {
  hasError: boolean;
};

export class FooterErrorBoundary extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError(): State {
    return { hasError: true };
  }

  render() {
    if (this.state.hasError) {
      return (
        <FooterPresenter mailAddress={null} externalServices={new Map()} />
      );
    }

    return this.props.children;
  }
}
