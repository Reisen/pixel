import React       from 'react';
import styles      from './ImagePanel.module.css';
import { image }   from '../../../../types/image';

interface Props {
    image: image
}

export default (props: Props) =>
    <div className={styles.Root}>
        <div className={styles.Gallery}>
            Gallery A
        </div>

        <div className={styles.Gallery}>
            Gallery B
        </div>

        <div className={styles.Gallery}>
            Gallery C
        </div>

        <div className={styles.Gallery}>
            Gallery D
        </div>

        <div className={styles.Gallery}>
            Gallery E
        </div>
    </div>;
