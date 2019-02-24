import React       from 'react';
import styles      from './ImagePanel.module.css';
import { image }   from '../../../../types/image';

interface Props {
    image: image
}

const ImagePanel = (props: Props) => {
    return (
        <img
            className={styles.Root}
            src="https://i.imgur.com/aG7C0Zx.jpg"
            alt="Focused"
        />
    );
}

export default ImagePanel;
